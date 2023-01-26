package mutex.system

import mutex.*
import org.slf4j.*
import java.io.*
import java.net.*
import java.util.concurrent.*

const val DEFAULT_IMPL_NAME = "ProcessImpl"

fun main(args: Array<String>) {
    val processId = args.getOrNull(0)?.toIntOrNull()
    val implName = args.getOrNull(1) ?: DEFAULT_IMPL_NAME
    if (args.size !in 1..2 || processId == null) {
        println("Usage: NodeKt <processId> [<impl-name>]")
        return
    }
    check(processId in 1..nodes.size)
    val incoming = LinkedBlockingQueue<Request>()
    val env = EnvironmentImpl(processId, incoming)
    val process = createProcess(env, implName)
    val console = NodeConsole(env)
    val server = Server(env, nodes[processId - 1])
    env.onStart(process)
    console.start()
    server.start()
    mainLoop(env, process)
    env.close()
    server.close()
}

private fun mainLoop(
    env: EnvironmentImpl,
    process: Process
) {
    while (true) {
        val req = env.incoming.take()
        try {
            when (req) {
                is MessageReq -> {
                    env.onMessage(req.srcId, req.message)
                    process.onMessage(req.srcId, req.message)
                }
                is ExitReq -> return
                is LockReq -> {
                    env.lockRequested = true
                    process.onLockRequest()
                }
                is UnlockReq -> {
                    env.unlockRequested = true
                    process.onUnlockRequest()
                }
                is PingReq -> env.onPing()
            }
        } catch (e: Throwable) {
            env.onError(e)
        }
    }
}

fun createProcess(env: Environment, implName: String): Process =
    Class.forName("mutex.$implName")
        .getConstructor(Environment::class.java)
        .newInstance(env) as Process

const val ACTION_TAG = "@"

private class EnvironmentImpl(
    override val processId: Int,
    val incoming: BlockingQueue<Request>
) : Environment {
    private val log = LoggerFactory.getLogger("Env")
    private val outConnections = nodes.map { node ->
        OutgoingConnection(this, node)
    }

    override val nProcesses: Int
        get() = nodes.size

    @Volatile
    var exitRequested: Boolean = false

    var lockRequested = false
    var unlockRequested = false

    fun onStart(process: Process) {
        log.info("Running node $processId out of ${nodes.size} with ${process::class.java.simpleName} class")
    }

    override fun send(destId: Int, message: Message) {
        log.info("$ACTION_TAG{$processId SEND $destId} $message")
        outConnections[destId - 1].send(message)
    }

    fun onMessage(srcId: Int, message: Message) {
        log.info("$ACTION_TAG{$processId RCVD $srcId} $message")
    }

    fun onError(e: Throwable) {
        log.error("$ACTION_TAG{$processId ERROR} in process", e)
    }

    override fun locked() {
        check(lockRequested) { "Lock was not requested" }
        lockRequested = false
        log.info("$ACTION_TAG{$processId LOCKED}")
    }

    override fun unlocked() {
        check(unlockRequested) { "Unlock was not requested" }
        unlockRequested = false
        log.info("$ACTION_TAG{$processId UNLOCKED}")
    }

    fun close() {
        log.info("Shutting down")
        outConnections.forEach { it.close() }
    }

    fun onPing() {
        log.info("$ACTION_TAG{$processId PONG}")
    }
}

private class NodeConsole(private val env: EnvironmentImpl) : Thread("Console") {
    private val log = LoggerFactory.getLogger("Console")

    init {
        isDaemon = true
    }

    override fun run() {
        loop@ while (true) {
            val line = readLine() ?: break
            when (line.trim().lowercase()) {
                "" -> {} // do nothing
                "exit" -> break@loop
                "lock" -> env.incoming.put(LockReq)
                "unlock" -> env.incoming.put(UnlockReq)
                "ping" -> env.incoming.put(PingReq)
                else -> log.error("Unrecognized line `$line`")
            }
        }
        env.exitRequested = true
        env.incoming.put(ExitReq)
    }
}

private const val MAGIC = 0x0A0B0C0D

private class OutgoingConnection(
    private val env: EnvironmentImpl,
    private val node: Node
) {
    private val log = LoggerFactory.getLogger("OutConn")

    private var socket: Socket? = null
    private var data: DataOutputStream? = null

    private fun open(): DataOutputStream {
        data?.let { return it }
        while (true) {
            if (env.exitRequested) error("Aborted due to exit request")
            log.info("Opening socket to $node")
            try {
                val socket = Socket(node.host, node.port)
                this.socket = socket
                val data = DataOutputStream(socket.getOutputStream().buffered())
                this.data = data
                data.writeInt(MAGIC)
                data.writeInt(env.processId)
                data.flush()
                return data
            } catch (e: Throwable) {
                log.info("Failed to open socket to node $node", e)
                close()
            }
            Thread.sleep(500) // delay before retry
        }
    }

    fun close() {
        try {
            data?.close()
            socket?.close()
        } finally {
            this.socket = null
            this.data = null
        }
    }

    @Synchronized
    fun send(message: Message) {
        val data = open()
        try {
            data.writeInt(message.bytes.size)
            data.write(message.bytes)
            data.flush()
        } catch (e: Throwable) {
            log.info("Failed to send packet to node $node", e)
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
        log.info("$ACTION_TAG{${node.id} LISTENING} at port ${node.port}")
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
            val data = DataInputStream(socket.getInputStream().buffered())
            check(data.readInt() == MAGIC) { "Expected MAGIC in incoming stream" }
            srcId = data.readInt()
            check(srcId in 1..server.env.nProcesses) { "Invalid incoming process id $srcId" }
            log.info("Accepted connection from remote process $srcId at $remoteAddr")
            while (true) {
                val size = data.readInt()
                val bytes = ByteArray(size)
                check(data.read(bytes) == size) { "Unexpected EOF" }
                server.received(MessageReq(srcId, Message(bytes)))
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

sealed class Request

class MessageReq(val srcId: Int, val message: Message) : Request()
object ExitReq : Request()
object LockReq : Request()
object UnlockReq : Request()
object PingReq : Request()
