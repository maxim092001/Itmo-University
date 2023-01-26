package mutex.system

import org.slf4j.*
import java.io.*
import java.util.*

fun main(args: Array<String>) {
    val sys = DistributedSystem(args)
    SystemConsole(sys).start()
    sys.awaitTermination()
}

val systemProps = Properties().also { props ->
    File("system.properties").inputStream().use { input ->
        props.load(input)
    }
}

val nodes: List<Node> = ArrayList<Node>().apply {
    var i = 0
    while (true) {
        val key = "node.${i + 1}"
        val addr = systemProps.getProperty(key) ?: break
        i++
        add(Node(i, addr.substringBefore(":"), addr.substringAfter(":").toInt()))
    }
}

data class Node(val id: Int, val host: String, val port: Int) {
    override fun toString(): String = "node.$id($host:$port)"
}

val javaHome = System.getProperty("java.home")!!
val classPath = System.getProperty("java.class.path")!!

open class DistributedSystem(args: Array<out String>) {
    protected val log: Logger = LoggerFactory.getLogger("System")
    private val procs: List<NodeProcess>

    init {
        log.info("Starting ${nodes.size} processes with ${args.toList()}")
        procs = nodes.map { node ->
            val process = ProcessBuilder()
                .command(javaHome / "bin" / "java",
                    "-classpath", classPath,
                    "mutex.system.NodeKt", node.id.toString(),
                    *args)
                .start()
            Reader(this, node, "inp", process.inputStream).start()
            Reader(this, node, "err", process.errorStream).start()
            NodeProcess(node, process)
        }
    }

    open fun onAction(id: Int, action: String) {
        // is overridden for automated testing
    }

    fun request(id: Int, msg: String) {
        procs.getOrNull(id - 1)?.request(msg)
    }

    fun reqExit() {
        log.info("Requesting exit for all nodes")
        reqAll("exit")
    }

    fun reqAll(msg: String) {
        procs.forEach {
            it.request(msg)
        }
    }

    open fun awaitTermination() {
        val waiters = procs.map { Waiter(it.node, it.process) }
        waiters.forEach { it.start() }
        waiters.forEach { it.join() }
        log.info("System has terminated")
    }
}

class NodeProcess(
    val node: Node,
    val process: Process
) {
    private val out = PrintStream(process.outputStream, true)

    fun request(msg: String) {
        println("out.${node.id} >> $msg")
        out.println(msg)
    }
}

private val ACTION_REGEX = Regex("$ACTION_TAG\\{([^}]+)}")

class Reader(
    private val sys: DistributedSystem,
    private val node: Node,
    name: String,
    inputStream: InputStream
) : Thread("$name.${node.id}") {
    private val inp = inputStream.bufferedReader()

    override fun run() {
        while (true) {
            val line = inp.readLine() ?: break
            println("$name << $line")
            val res = ACTION_REGEX.find(line)
            if (res != null) sys.onAction(node.id, res.groups[1]!!.value)
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

    override fun run() {
        loop@ while (true) {
            val line = readLine()?.trim() ?: continue
            when (line.lowercase()) {
                "exit" -> break@loop
                "ping" -> sys.reqAll("ping")
                "lock" -> sys.reqAll("lock")
                else -> {
                    val id = line.substringBefore(' ').toIntOrNull() ?: continue@loop
                    sys.request(id, line.substringAfter(' '))
                }
            }
        }
        sys.reqExit()
    }
}