package raft.system

import org.slf4j.*
import java.io.*
import java.util.*
import java.util.concurrent.ConcurrentHashMap

fun main(args: Array<String>) {
    val sys = DistributedSystem(Properties(), args)
    SystemConsole(sys).start()
}

data class Node(val id: Int, val host: String, val port: Int) {
    override fun toString(): String = "node.$id($host:$port)"
}

val javaHome = System.getProperty("java.home")!!
val classPath = System.getProperty("java.class.path")!!

enum class LogMode { SINGLE, ON, OFF }

open class DistributedSystem(
    private val props: Properties,
    private val args: Array<out String>
) {
    protected val log: Logger = LoggerFactory.getLogger("System")
    protected val nProcesses = Configuration.nodes.size

    private val procs = ConcurrentHashMap<Node, NodeProcess>()
    private val logOn = ConcurrentHashMap<Node, Boolean>()

    init {
        log.info("Starting $nProcesses processes with ${args.toList()}")
        for (node in Configuration.nodes) startProcess(node)
        Runtime.getRuntime().addShutdownHook(Thread {
            for (proc in procs.values) proc.process.destroy()
        })
    }

    private fun startProcess(node: Node) {
        if (procs.containsKey(node)) return // already active
        val process = ProcessBuilder()
            .command(
                javaHome / "bin" / "java",
                "-classpath", classPath,
                *props.map { (k, v) -> "-D$k=$v" }.toTypedArray(),
                "raft.system.NodeKt", node.id.toString(),
                *args
            )
            .start()
        procs[node] = NodeProcess(node, process)
        Reader(this, node, "err", process.errorStream).start()
        Reader(this, node, "inp", process.inputStream, onStop = {
            log.info("Process ${node.id} has stopped")
            procs.remove(node)
        }).start()
    }

    private fun nodeById(id: Int) = Configuration.nodes.getOrNull(id - 1)
    private fun procById(id: Int): NodeProcess? = nodeById(id)?.let { procs[it] }

    fun startProcess(id: Int) {
        nodeById(id)?.let { startProcess(it) }
    }

    fun isLogOn(node: Node): Boolean = logOn[node] ?: true

    fun setLogMode(id: Int, mode: LogMode) {
        val node = nodeById(id) ?: return
        when (mode) {
            LogMode.ON -> logOn[node] = true
            LogMode.OFF -> logOn[node] = false
            LogMode.SINGLE -> for (n in Configuration.nodes) logOn[n] = n == node
        }
    }

    open fun onAction(id: Int, action: String, attachment: String) {
        // is overridden for automated testing
    }

    fun request(id: Int, msg: String) {
        procById(id)?.request(msg)
    }

    fun reqExit() {
        log.info("Requesting stop for all nodes")
        reqAll("stop")
    }

    fun reqAll(msg: String) {
        procs.values.forEach {
            it.request(msg)
        }
    }

    open fun awaitTermination() {
        val waiters = procs.values.map { Waiter(it.node, it.process) }
        waiters.forEach { it.start() }
        waiters.forEach { it.join() }
        log.info("System has terminated")
    }
}

class NodeProcess(
    val node: Node,
    val process: Process,
) {
    private val out = PrintStream(process.outputStream, true)

    fun request(msg: String) {
        println("out.${node.id} >> $msg")
        out.println(msg)
    }
}

private val ACTION_REGEX = Regex("$ACTION_TAG\\{([^}]+)}(.*)")

class Reader(
    private val sys: DistributedSystem,
    private val node: Node,
    name: String,
    inputStream: InputStream,
    private val onStop: () -> Unit = {}
) : Thread("$name.${node.id}") {
    private val inp = inputStream.bufferedReader()

    override fun run() {
        try {
            while (true) {
                val line = inp.readLine() ?: break
                if (sys.isLogOn(node)) println("$name << $line")
                val res = ACTION_REGEX.find(line)
                if (res != null) sys.onAction(node.id, res.groups[1]!!.value, res.groups[2]!!.value.trim())
            }
        } finally {
            onStop()
        }
    }
}

class Waiter(
    private val node: Node,
    private val process: Process
) : Thread("Waiter-${node.id}") {
    private val log = LoggerFactory.getLogger("Waiter")

    override fun run() {
        val exitCode = process.waitFor()
        log.info("Process for $node exited with code $exitCode")
    }
}

private operator fun String.div(sub: String) = this + File.separator + sub

class SystemConsole(private val sys: DistributedSystem) : Thread("Console") {
    init {
        isDaemon = true
    }

    private fun help() {
        println("""
            <pid> log               -- show log ONLY from process <pid>, turn off the rest 
            <pid> log off           -- turn off logging from process <pid>
            <pid> log on            -- turn on logging from process <pid>
            <pid> set <key> <value> -- sets <key> to <value> at process <pid>
            <pid> dump              -- dumps state machine at process <pid>
            <pid> stop              -- shutdown the process <pid>
            <pid> start             -- restart the process <pid>
            dump                    -- dumps state machines of all processes
            exit                    -- shutdown the system
        """.trimIndent())
    }

    override fun run() {
        loop@ while (true) {
            val line = readLine()?.trim() ?: break
            when (line.lowercase()) {
                "exit" -> break@loop
                "dump" -> sys.reqAll("dump")
                else -> {
                    val id = line.substringBefore(' ').toIntOrNull()
                    if (id == null) {
                        help()
                        continue@loop
                    }
                    val cmd = line.substringAfter(' ').trim()
                    val cmds = cmd.split(" ")
                    when (cmds[0].lowercase()) {
                        "start" -> sys.startProcess(id)
                        "log" -> {
                            val mode = when (cmds.getOrNull(1)?.lowercase() ?: "") {
                                "" -> LogMode.SINGLE
                                "on" -> LogMode.ON
                                "off" -> LogMode.OFF
                                else -> { help(); continue@loop }
                            }
                            sys.setLogMode(id, mode)
                        }
                        else -> sys.request(id, cmd)
                    }
                }
            }
        }
        sys.reqExit()
    }
}