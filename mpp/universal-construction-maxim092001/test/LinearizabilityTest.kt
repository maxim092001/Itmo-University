import org.jetbrains.kotlinx.lincheck.LinChecker
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressCTest
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import kotlin.test.Test


@StressCTest
class LinearizabilityTest : VerifierState() {
    private val c = Solution()

    @Operation
    fun add(x: Int) = c.getAndAdd(x)

    @Test
    fun testLinearizability() {
        LinChecker.check(LinearizabilityTest::class.java)
    }

    override fun extractState(): Any = c.getAndAdd(0)
}