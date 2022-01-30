import org.jetbrains.kotlinx.lincheck.LinChecker
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.annotations.Param
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.jetbrains.kotlinx.lincheck.paramgen.LongGen
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressCTest
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test
import java.util.*

/**
 * This test checks bank implementation for linearizability.
 */
@Param.Params(
    Param(name = "id", gen = IntGen::class, conf = "0:4"),
    Param(name = "amount", gen = LongGen::class, conf = "1:100")
)
@StressCTest
class LinearizabilityTest : VerifierState() {
    private val bank: Bank = BankImpl(5)

    @Operation(params = ["id"])
    fun getAmount(id: Int): Long =
        bank.getAmount(id)

    @get:Operation
    val totalAmount: Long
        get() = bank.totalAmount

    @Operation(params = ["id", "amount"], handleExceptionsAsResult = [IllegalStateException::class])
    fun deposit(id: Int, amount: Long): Long =
        bank.deposit(id, amount)

    @Operation(params = ["id", "amount"], handleExceptionsAsResult = [IllegalStateException::class])
    fun withdraw(id: Int, amount: Long): Long =
        bank.withdraw(id, amount)

    @Operation(params = ["id", "id", "amount"], handleExceptionsAsResult = [IllegalStateException::class])
    fun transfer(idFrom: Int, idTo: Int, amount: Long) {
        if (idFrom != idTo) bank.transfer(idFrom, idTo, amount)
    }

    override fun extractState(): Any {
        val amounts: MutableList<Long> = ArrayList()
        for (i in 0 until bank.numberOfAccounts) amounts.add(bank.getAmount(i))
        return amounts
    }

    @Test
    fun test() {
        LinChecker.check(LinearizabilityTest::class.java)
    }
}