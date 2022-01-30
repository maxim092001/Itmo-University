import java.util.concurrent.atomic.*
import kotlin.random.*
import kotlin.test.*

private const val N = 100
private const val MEAN = 1000000000L
private const val AMT = 1000 // AMT << MEAN, so that probability of over/under flow is negligible
private const val MOD = 100 // all deposits / withdrawals are divisible by MOD

class BankRunner {
    private val bank: Bank = BankImpl(N)
    private val expected = Array(N) { AtomicLong(MEAN) }
    private var dummy = 0L // will prevent code elimination

    init {
        assertEquals(N, bank.numberOfAccounts)
        for (i in 0 until N) bank.deposit(i, MEAN)
        for (i in 0 until N) assertEquals(MEAN, bank.getAmount(i))
    }
    
    fun runOperation() {
        val op = Random.nextInt(100)
        if (op == 0) { // every 100th operation on average is getTotalAmount
            val totalAmount = bank.totalAmount
            assertEquals(0, totalAmount % MOD) // the result must be divisible to MOD
            return
        }
        when (op and 3) {
            0 -> {
                val i = Random.nextInt(N)
                val amount = nextRoundAmount()
                bank.deposit(i, amount)
                expected[i].addAndGet(amount)
            }
            1 -> {
                val i = Random.nextInt(N)
                val amount = nextRoundAmount()
                bank.withdraw(i, amount)
                expected[i].addAndGet(-amount)
            }
            2 -> {
                var i: Int
                var j: Int
                do {
                    i = Random.nextInt(N)
                    j = Random.nextInt(N)
                } while (i == j)
                val amount = nextAmount() // an arbitrary amount is transferred between accounts
                bank.transfer(i, j, amount)
                expected[i].addAndGet(-amount)
                expected[j].addAndGet(amount)
            }
            3 ->  {
                // NOTE: the result does not have to be equal to expected.
                val i = Random.nextInt(N)
                dummy = bank.getAmount(i)
            }
        }
    }

    fun verifyState() {
        var expectedTotal: Long = 0
        for (i in 0 until N) {
            val ei = expected[i].get()
            assertEquals(ei, bank.getAmount(i))
            expectedTotal += ei
        }
        assertEquals(expectedTotal, bank.totalAmount)
    }

    private fun nextRoundAmount(): Long = (nextAmount() + MOD - 1) / MOD * MOD
    private fun nextAmount(): Long = (Random.nextInt(AMT) + 1).toLong()

}