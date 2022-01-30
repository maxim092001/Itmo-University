import kotlinx.atomicfu.*
import kotlin.random.*
import kotlin.test.*

class LockFreedomTest {
    private val env = LockFreedomTestEnvironment("LockFreedomTest")
    private val nThreads = 3
    private val nSeconds = 3

    @Test
    fun testLockFreedom() {
        val c = Solution()
        repeat(nThreads) { index ->
            env.testThread("adder-$index") {
                c.getAndAdd(Random.nextInt())
            }
        }
        env.performTest(nSeconds) {}
    }
}