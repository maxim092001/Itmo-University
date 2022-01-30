import java.lang.reflect.*
import java.util.*
import java.util.concurrent.atomic.*
import java.util.concurrent.locks.*
import kotlin.system.*

const val TEST_TIME_SEC = 10 // number of seconds to test
const val N_CLOCKS = 2 // test two clocks concurrently
const val PAUSE_EVERY_N_STEPS = 1000 // lock-freedom testing

fun error(text: String): Nothing  {
    println("ERROR: $text")
    exitProcess(1)
}

fun verifyClass(clazz: Class<Solution>) {
    if (clazz.superclass != Any::class.java) error("Solution cannot base base classes")
    for (f in clazz.declaredFields) {
        if (Modifier.isStatic(f.modifiers)) continue
        if (f.type != RegularInt::class.java) error("Solution fields must be of RegularInt type: $f")
        if (!Modifier.isFinal(f.modifiers)) error("Solution fields must be final: $f")
    }
}

fun updateStep() {
    val thread = Thread.currentThread() as? TestThread ?: return
    thread.stepImpl()
}

private enum class Status { RUNNING, DONE }

private val status = AtomicReference(Status.RUNNING)

private class Group(private val index: Int) {
    private val paused = AtomicReference<TestThread?>()
    private var lastProgress = -1
    private val _ticks = AtomicInteger()
    val ticks: Int get() = _ticks.get()
    @Volatile var beforeWrite = Time(0, 0, 0)
    @Volatile var afterWrite = Time(0, 0, 0)

    fun pause(thread: TestThread) {
        if (!paused.compareAndSet(null, thread)) return
        while (paused.get() == thread) LockSupport.park()
    }

    fun tick() {
        _ticks.incrementAndGet()
        val thread = paused.getAndSet(null) ?: return
        LockSupport.unpark(thread)
    }

    fun verify() {
        val progress = ticks
        if (progress == lastProgress) error("No progress in group $index")
        lastProgress = progress
    }
}

private open class TestThread(name: String, val group: Group) : Thread(name) {
    val random = Random()
    fun stepImpl() {
        if (random.nextInt(PAUSE_EVERY_N_STEPS) == 0) group.pause(this)
    }
}

private inline fun testThread(name: String, group: Group, crossinline block: () -> Unit) =
    object : TestThread(name, group) {
        override fun run() {
            while (status.get() == Status.RUNNING) {
                block()
                group.tick()
            }
        }
    }

fun main() {
    verifyClass(Solution::class.java)
    val solutions = List(N_CLOCKS) { Solution() }
    val groups = List(N_CLOCKS) { i -> Group(i) }
    val baseTime = System.nanoTime()
    val writers = List<Thread>(N_CLOCKS) { i ->
        testThread("Writer$i", groups[i]) {
            val cur = System.nanoTime() - baseTime
            // d1 += every 100 ms
            // d2 += every 10 us
            // d3 += every ns
            val time = Time((cur / 100_000_000).toInt(), (cur % 100_000_000 / 10_000).toInt(), (cur % 10_000).toInt())
            groups[i].beforeWrite = time
            solutions[i].write(time)
            groups[i].afterWrite = time
        }
    }
    val readers = List<Thread>(N_CLOCKS) { i ->
        var last = Time(0, 0, 0)
        testThread("Reader$i", groups[i]) {
            val lastWrite = groups[i].afterWrite
            val cur = solutions[i].read()
            val startWrite = groups[i].beforeWrite
            if (cur < last) error("Non-monotonic read $cur < $last")
            if (cur < lastWrite) error("Read value earlier than last written one $cur < $lastWrite")
            if (cur > startWrite) error("Read value later than value being written $cur > $startWrite")
            last = cur
        }
    }
    (readers + writers).forEach { it.start() }
    repeat(TEST_TIME_SEC) { sec ->
        println("#$sec: Running progress = ${groups.sumBy { it.ticks }}")
        groups.forEach { it.verify() }
        Thread.sleep(1000)
    }
    status.set(Status.DONE)
    groups.forEach { it.tick() }
    (readers + writers).forEach { it.join() }
    println("ACCEPTED")
    exitProcess(0)
}