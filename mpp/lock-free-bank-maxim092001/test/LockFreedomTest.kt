import kotlinx.atomicfu.*
import kotlin.test.*

private const val N_THREADS = 4
private const val N_SECONDS = 5

class LockFreedomTest {
    private val env = LockFreedomTestEnvironment("LockFreedomTest")
    private val br = BankRunner()

    @Test
    fun testLockFreedom() {
        repeat(N_THREADS) {
            env.testThread {
                br.runOperation()
            }
        }
        env.performTest(N_SECONDS) {}
        br.verifyState()
    }
}