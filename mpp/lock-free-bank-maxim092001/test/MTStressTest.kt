import java.util.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import kotlin.math.*
import kotlin.test.*

private const val THREADS = 10
private const val TOTAL_PHASES = 10
private const val INIT_PHASES = 5
private const val PHASE_DURATION_MILLIS = 1000L

/**
 * Multi-threaded stress test for bank implementation -- many threads and operations of various accounts.
 *
 * TestS correctness of concurrent deposit, withdraw, transfer, and getTotalAmount operations.
 * It does not check getAmount operations concurrently with the above.
 */
class MTStressTest {
    private val phaser = Phaser(1 + THREADS)
    private val totalOps = AtomicLong() // only non-init phases are counted
    private val br = BankRunner()

    @Volatile
    private var failed = false

    @Test
    fun testStress() {
        val ts = Array(THREADS) { threadNo -> TestThread(threadNo) }
        ts.forEach { it.start() }
        val stats = Stats()
        var phase = 1
        while (!failed && phase <= TOTAL_PHASES) {
            System.out.printf("Phase #%2d - ", phase)
            totalOps.set(0)
            if (phaser.arriveAndAwaitAdvance() < 0) break
            if (phaser.arriveAndAwaitAdvance() < 0) break
            val ops = totalOps.get()
            if (phase > INIT_PHASES) {
                stats.add(ops)
                System.out.printf(Locale.US, "done %,10d ops (measure)%n", ops)
            } else {
                System.out.printf(Locale.US, "done %,10d ops (warm up)%n", ops)
            }
            phase++
        }
        ts.forEach { it.join() }
        assertFalse(failed)
        println("Average ops per phase: $stats")
    }

    private inner class TestThread(threadNo: Int) : Thread("TestThread-$threadNo") {
        override fun run() {
            try {
                var phase = 1
                while (!failed && phase <= TOTAL_PHASES) {
                    runPhase()
                    phase++
                }
            } catch (t: Throwable) {
                t.printStackTrace()
                failed = true
                phaser.forceTermination()
            }
        }

        private fun runPhase() {
            br.verifyState()
            if (phaser.arriveAndAwaitAdvance() < 0) return
            var ops = 0
            val tillTimeMillis = System.currentTimeMillis() + PHASE_DURATION_MILLIS
            do {
                br.runOperation()
                ops++
            } while (System.currentTimeMillis() < tillTimeMillis)
            totalOps.addAndGet(ops.toLong())
            phaser.arriveAndAwaitAdvance()
        }
    }
}

class Stats {
    var n = 0
    var mean = 0.0
    val dev: Double
        get() = sqrt(nVar / (n - 1))

    private var nVar = 0.0

    fun add(x: Long) {
        n++
        val prev = mean
        mean += (x - prev) / n
        nVar += (x - prev) * (x - mean)
    }

    override fun toString(): String = String.format(Locale.US, "%,.0f +- %,.0f", mean, dev)
}