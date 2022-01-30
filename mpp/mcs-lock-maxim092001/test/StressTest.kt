import org.junit.*
import java.util.concurrent.atomic.*
import java.util.concurrent.locks.*
import kotlin.random.*

class StressTest : TestBase(), Environment {
    private val count = AtomicLong()
    
    @Test
    fun testStress() {
        val rs = start(5, Solution(this))
        var old = 0L
        for (second in 1..3) {
            Thread.sleep(1000)
            val new = count.get()
            println("$second: Working $new")
            require(new > old) { "Should have progress" }
            old = new
        }
        stop(rs)
    }

    private fun <N> start(n: Int, lock: Lock<N>): List<Runner<*>> {
        val rs = List<Runner<*>>(n) { i -> Runner(lock, "Runner-$i") }
        rs.configureExceptionHandlers()
        rs.forEach { it.start() }
        return rs
    }

    private fun stop(rs: List<Runner<*>>) {
        rs.forEach { it.interrupt() }
        rs.forEach { it.join() }
    }

    inner class Runner<N>(val lock: Lock<N>, name: String) : Thread(name) {
        override fun run() {
            while (!interrupted()) {
                lock.checkLock {
                    count.incrementAndGet()
                    spin()
                }
                spin()
            }
        }

        private tailrec fun spin(): Unit =
            when (Random.nextInt(10)) {
                in 0..4 -> Unit
                in 5..8 -> spin()
                else -> yield()
            }
    }

    override fun park() = LockSupport.park()
    override fun unpark(thread: Thread) = LockSupport.unpark(thread)
}