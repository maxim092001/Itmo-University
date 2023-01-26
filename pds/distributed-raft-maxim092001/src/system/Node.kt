package raft.system

import kotlinx.serialization.*
import kotlinx.serialization.json.*
import org.slf4j.*
import raft.*
import java.io.*
import java.net.*
import java.util.concurrent.*
import kotlin.random.*
import raft.system.ActionTag.*
import raft.system.Request.*
import java.lang.reflect.*

const val DEFAULT_IMPL_NAME = "ProcessImpl"
const val ACTION_TAG = "@"
const val STORAGE_FQN_PROP = "storageFqn"
const val MACHINE_FQN_PROP = "machineFqn"

enum class ActionTag { LISTENING, DUMP, SENT, RCVD, COMMAND, RESULT, ERROR, TIMEOUT, RESTART, COMMIT }

val json = Json.Default

private const val MAGIC_HEADER = "RAFT-NODE"

fun main(args: Array<String>) {
    val processId = args.getOrNull(0)?.toIntOrNull()
    val implName = args.getOrNull(1) ?: DEFAULT_IMPL_NAME
    if (args.size !in 1..2 || processId == null) {
        println("Usage: NodeKt <processId> [<impl-name>]")
        return
    }
    check(processId in 1..Configuration.nodes.size)
    val incoming = LinkedBlockingQueue<Request>()
    val storage = createImpl<Storage>(processId, STORAGE_FQN_PROP) { PersistentStorage(processId) }
    val machine = createStateMachine(processId)
    val env = EnvironmentImpl(processId, incoming, storage, machine)
    env.onStart(implName)
    val console = NodeConsole(env)
    val server = Server(env, Configuration.nodes[processId - 1])
    console.start()
    server.start()
    mainLoop(env, implName)
    env.close()
    server.close()
}

private fun createStateMachine(processId: Int) = createImpl(processId, MACHINE_FQN_PROP) { StateMachine() }

@Suppress("UNCHECKED_CAST")
private fun <T> createImpl(processId: Int, prop: String, default: () -> T): T =
    System.getProperty(prop)?.takeIf { it.isNotEmpty() }?.let result@{
        val clazz = Class.forName(it)
        clazz.findConstructor(Int::class.java)?.let { return@result it.newInstance(processId) as T }
        clazz.findConstructor()?.newInstance() as T
    } ?: default()

private fun Class<*>.findConstructor(vararg params: Class<*>): Constructor<*>? =
    try { getConstructor(*params) } catch (e: NoSuchMethodException) { null }

private fun mainLoop(
    env: EnvironmentImpl,
    implName: String
) {
    var process = createProcess(env, implName)
    while (true) {
        val waitTime = env.checkTimeout()
        val req = env.incoming.poll(waitTime, TimeUnit.MILLISECONDS) ?: continue
        try {
            when (req) {
                is MessageReq -> {
                    env.onMessage(req.srcId, req.message)
                    process.onMessage(req.srcId, req.message)
                }
                is CommandReq -> {
                    env.onClientCommand(req.command)
                    process.onClientCommand(req.command)
                }
                is TimeoutReq -> {
                    env.onTimeout()
                    process.onTimeout()
                }
                is DumpReq -> env.onDump()
                is RestartReq -> {
                    env.onRestart()
                    process = createProcess(env, implName)
                }
                is ExitReq -> return
            }
        } catch (e: Throwable) {
            env.onError(e)
        }
    }
}

fun createProcess(env: Environment, implName: String): Process =
    Class.forName("raft.$implName")
        .getConstructor(Environment::class.java)
        .newInstance(env) as Process

private class EnvironmentImpl(
    override val processId: Int,
    val incoming: BlockingQueue<Request>,
    override val storage: Storage,
    override var machine: StateMachine
) : Environment {
    private val log = LoggerFactory.getLogger("Env")
    private val outConnections = Configuration.nodes.map { node ->
        OutgoingConnection(this, node)
    }

    override val nProcesses: Int
        get() = Configuration.nodes.size

    @Volatile
    var exitRequested: Boolean = false

    var nextTimeout = Long.MAX_VALUE
    var timeout = Timeout.ELECTION_TIMEOUT

    fun onStart(implName: String) {
        log.info("Running node $processId out of $nProcesses with $implName class")
    }

    override fun send(destId: Int, message: Message) {
        log.info("$ACTION_TAG{$processId $SENT $destId} ${json.encodeToString(message)}")
        outConnections[destId - 1].send(message)
    }

    fun onMessage(srcId: Int, message: Message) {
        log.info("$ACTION_TAG{$processId $RCVD $srcId} ${json.encodeToString(message)}")
    }

    fun onClientCommand(command: Command) {
        log.info("$ACTION_TAG{$processId $COMMAND} ${json.encodeToString(command)})}")
    }

    override fun onClientCommandResult(result: CommandResult) {
        log.info("$ACTION_TAG{$processId $RESULT} ${json.encodeToString(result)}")
    }

    fun onError(e: Throwable) {
        log.error("$ACTION_TAG{$processId $ERROR} in process", e)
    }

    override fun startTimeout(timeout: Timeout) {
        val now = System.currentTimeMillis()
        val heartbeatTimeoutMs = Configuration.heartbeatTimeoutMs
        nextTimeout = now + heartbeatTimeoutMs + when (timeout) {
            Timeout.ELECTION_TIMEOUT ->
                if (Configuration.heartbeatRandom) Random.nextInt(heartbeatTimeoutMs / nProcesses, heartbeatTimeoutMs)
                else processId * heartbeatTimeoutMs / nProcesses
            Timeout.LEADER_HEARTBEAT_PERIOD -> 0
        }
        this.timeout = timeout
    }

    fun checkTimeout(): Long {
        val now = System.currentTimeMillis()
        if (now < nextTimeout) return nextTimeout - now
        nextTimeout = Long.MAX_VALUE
        incoming.put(TimeoutReq)
        return 0
    }

    fun onTimeout() {
        log.info("$ACTION_TAG{$processId $TIMEOUT} $timeout")
    }

    fun close() {
        log.info("Shutting down")
        outConnections.forEach { it.close() }
    }

    fun onDump() {
        log.info("$ACTION_TAG{$processId DUMP} $machine")
    }

    fun onRestart() {
        log.info("$ACTION_TAG{$processId $RESTART}")
        machine = createStateMachine(processId)
    }
}

private class NodeConsole(private val env: EnvironmentImpl) : Thread("Console") {
    private val log = LoggerFactory.getLogger("Console")
    private var lastCommandId = 0

    init {
        isDaemon = true
    }

    private fun help() {
        println("""
            set <key> <value> -- sets <key> to <value>
            dump              -- dumps state machine
            restart           -- restart the process's code
            stop              -- shutdown the process
        """.trimIndent())
    }

    override fun run() {
        while (!env.exitRequested) {
            val line = readLine() ?: "exit"
            try {
                processLine(line)
            } catch (e: Throwable) {
                log.error("Error processing '$line'", e)
            }
        }
    }

    private fun processLine(line: String) {
        val s = line.trim().split(" ").toMutableList()
        fun takeCommandId(): Int {
            val cmdId = s.getOrNull(1)
            if (cmdId != null && cmdId.length >= 3 && cmdId.startsWith("#") && cmdId.endsWith("#")) {
                s.removeAt(1)
                return cmdId.substring(1, cmdId.length - 1).toInt()
            }
            return ++lastCommandId
        }
        when (s[0].lowercase()) {
            "" -> help()
            "set" -> {
                val commandId = takeCommandId()
                if (s.size != 3) {
                    log.error("Expected 'set <key> <value>'")
                } else {
                    env.incoming.put(CommandReq(Command(env.processId, commandId, s[1], s[2])))
                }
            }
            "dump" -> env.incoming.put(DumpReq)
            "restart" -> env.incoming.put(RestartReq)
            "stop" -> {
                env.exitRequested = true
                env.incoming.put(ExitReq)
            }
            else -> log.error("Unrecognized line `$line`, press ENTER for help.")
        }
    }
}

private class OutgoingConnection(
    private val env: EnvironmentImpl,
    private val node: Node
) {
    private val log = LoggerFactory.getLogger("OutConn")

    private var socket: Socket? = null
    private var out: BufferedWriter? = null

    private fun open(): BufferedWriter? {
        out?.let { return it }
        if (env.exitRequested) error("Aborted due to exit request")
        log.info("Opening socket to $node")
        try {
            val socket = Socket(node.host, node.port)
            this.socket = socket
            val out = socket.getOutputStream().bufferedWriter()
            this.out = out
            out.write("$MAGIC_HEADER ${env.processId}\n")
            out.flush()
        } catch (e: Throwable) {
            log.info("Failed to open socket to node $node: $e")
            close()
        }
        return out
    }

    fun close() {
        try {
            out?.close()
            socket?.close()
        } finally {
            this.socket = null
            this.out = null
        }
    }

    @Synchronized
    fun send(message: Message) {
        val out = open() ?: return
        try {
            val encoded = json.encodeToString(message)
            out.write("$encoded\n")
            out.flush()
        } catch (e: Throwable) {
            log.info("Failed to send packet to node $node: $e")
            close()
        }
    }
}

private class Server(
    val env: EnvironmentImpl,
    private val node: Node
) : Thread("Server-${node.port}") {
    private val log = LoggerFactory.getLogger("Server")

    @Volatile
    private var closed = false

    private val serverSocket = ServerSocket(node.port)
    private val inConnections: MutableSet<IncomingConnection> = LinkedHashSet()

    override fun run() {
        log.info("$ACTION_TAG{${node.id} $LISTENING} at port ${node.port}")
        loop@while (true) {
            val socket = try {
                serverSocket.accept()
            } catch (e: SocketException) {
                if (closed) break@loop else throw e
            }
            val remoteAddr = "${socket.inetAddress.hostAddress}:${socket.port}"
            val connection = IncomingConnection(this, socket, remoteAddr)
            if (addConnection(connection)) {
                connection.start()
            } else {
                connection.close()
            }
        }
    }

    fun received(packet: MessageReq) {
        env.incoming.put(packet)
    }
    
    @Synchronized
    fun addConnection(connection: IncomingConnection): Boolean {
        inConnections += connection
        return !closed
    }

    @Synchronized
    fun removeConnection(connection: IncomingConnection) {
        inConnections -= connection
    }

    @Synchronized
    fun close() {
        closed = true
        serverSocket.close()
        inConnections.toList().forEach { it.close() }
    }
}

private class IncomingConnection(
    private val server: Server,
    private val socket: Socket,
    private val remoteAddr: String
) : Thread("InConn-$remoteAddr") {
    private val log = LoggerFactory.getLogger("InConn")

    private var closed = false
    
    override fun run() {
        var srcId = -1
        try {
            val input = socket.getInputStream().bufferedReader()
            val header = input.readLine().split(" ")
            check(header[0] == MAGIC_HEADER) { "Expected '$MAGIC_HEADER' in incoming stream" }
            srcId = header[1].toInt()
            check(srcId in 1..server.env.nProcesses) { "Invalid incoming process id $srcId" }
            log.info("Accepted connection from remote process $srcId at $remoteAddr")
            while (true) {
                val line = input.readLine() ?: break
                server.received(MessageReq(srcId, json.decodeFromString(line)))
            }
        } catch (e: EOFException) {
            /* do nothing -- assume this is normal termination */
        } catch (e: SocketException) {
            /* do nothing -- assume this is normal termination */
        } catch (e: Throwable) {
            log.error("Exception while processing incoming connection", e)
        } finally {
            close()
            log.info("Closed connection with remote process $srcId at $remoteAddr")
        }
    }

    fun close() {
        synchronized(server) {
            if (closed) return
            closed = true
            server.removeConnection(this)
            socket.close()
        }
    }
}

sealed class Request {
    class MessageReq(val srcId: Int, val message: Message) : Request()
    class CommandReq(val command: Command) : Request()
    object TimeoutReq : Request()
    object DumpReq : Request()
    object RestartReq : Request()
    object ExitReq : Request()
}

