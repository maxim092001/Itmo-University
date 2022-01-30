import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicLong
import kotlin.test.*

private const val N = 100
private const val MEAN: Long = 1000000000
private const val AMT = 1000 // AMT << MEAN, so that probability of over/under flow is negligible
private const val MOD = 100 // all deposits / withdrawals are divisible by MOD
private const val THREADS = 16
private const val TOTAL_PHASES = 20
private const val INIT_PHASES = 10
private const val PHASE_DURATION_MILLIS: Long = 1000

/**
 * Multi-threaded stress test for bank implementation -- many threads and operations of various accounts.
 *
 * This test test correctness of concurrent deposit, withdraw, transfer, and getTotalAmount operations.
 * It does not check getAmount operations concurrently with the above.
 */
class MTStressTest  {
    private val phaser = Phaser(1 + THREADS)
    private val bank: Bank = BankImpl(N)
    private val expected = arrayOfNulls<AtomicLong>(N)
    private val totalOps = AtomicLong() // only non-init phases are counted

    @Volatile
    private var failed = false

    private var dummy = 0L // will prevent code elimination

    @Test
    fun testStress() {
        assertEquals(N, bank.numberOfAccounts)
        for (i in 0 until N) bank.deposit(i, MEAN)
        for (i in 0 until N) assertEquals(MEAN, bank.getAmount(i))
        for (i in 0 until N) expected[i] = AtomicLong(MEAN)
        val ts = arrayOfNulls<TestThread>(N)
        for (threadNo in 0 until THREADS) {
            val t = TestThread(threadNo)
            ts[threadNo] = t
            t.start()
        }
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
            } else System.out.printf(Locale.US, "done %,10d ops (warm up)%n", ops)
            phase++
        }
        for (threadNo in 0 until THREADS) ts[threadNo]!!.join()
        assertFalse(failed)
        println("Average ops per phase: $stats")
    }

    private inner class TestThread(private val threadNo: Int) : Thread("TestThread-$threadNo") {
        private var rnd: ThreadLocalRandom? = null
        override fun run() {
            rnd = ThreadLocalRandom.current()
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
            verifyState()
            if (phaser.arriveAndAwaitAdvance() < 0) return
            var ops = 0
            val tillTimeMillis = System.currentTimeMillis() + PHASE_DURATION_MILLIS
            do {
                runOperation()
                ops++
            } while (System.currentTimeMillis() < tillTimeMillis)
            totalOps.addAndGet(ops.toLong())
            phaser.arriveAndAwaitAdvance()
        }

        private fun verifyState() {
            var expectedTotal: Long = 0
            for (i in 0 until N) {
                val ei = expected[i]!!.get()
                assertEquals(ei, bank.getAmount(i))
                expectedTotal += ei
            }
            assertEquals(expectedTotal, bank.totalAmount)
        }

        private fun runOperation() {
            val op = rnd!!.nextInt(100)
            if (op == 0) {
                // every 100th operation on average is getTotalAmount
                val totalAmount = bank.totalAmount
                assertEquals(0, totalAmount % MOD) // the result must be divisible to MOD
                return
            }
            val i = rnd!!.nextInt(N)
            val amount: Long
            when (op and 3) {
                0 -> {
                    amount = nextRoundAmount()
                    bank.deposit(i, amount)
                    expected[i]!!.addAndGet(amount)
                }
                1 -> {
                    amount = nextRoundAmount()
                    bank.withdraw(i, amount)
                    expected[i]!!.addAndGet(-amount)
                }
                2 -> {
                    var j = rnd!!.nextInt(N - 1)
                    if (j >= i) j++
                    // arbitrary amount is transferred between accounts
                    amount = nextAmount().toLong()
                    bank.transfer(i, j, amount)
                    expected[i]!!.addAndGet(-amount)
                    expected[j]!!.addAndGet(amount)
                }
                3 ->                     // NOTE: the result does not have to be equal to expected.
                    dummy = bank.getAmount(i)
            }
        }

        private fun nextRoundAmount(): Long {
            return (nextAmount().toLong() + MOD - 1) / MOD * MOD
        }

        private fun nextAmount(): Int {
            return rnd!!.nextInt(AMT) + 1
        }
    }
}