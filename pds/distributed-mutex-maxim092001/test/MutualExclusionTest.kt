package mutex.system

import kotlin.random.*
import kotlin.test.*

class MutualExclusionTest {
    private val sys = TestSystem(System.getProperty("implName") ?: DEFAULT_IMPL_NAME)
    private val skipCountCheck = System.getProperty("skipCountCheck")?.toBoolean() ?: false
    private val repeatCount = 100
    private val rnd = Random(1)
    private val n = nodes.size

    @BeforeTest
    fun setup() {
        sys.awaitListening()
    }

    @AfterTest
    fun tearDown() {
        sys.reqExit()
        sys.awaitTermination()
        sys.checkNotFailed()
    }

    @Test
    fun testMutex() {
        // repeat the whole test suite
        repeat(repeatCount) {
            val cnt = sys.awaitPingPongAndCountMessages()
            check(skipCountCheck || cnt == 0) { "$cnt messages without lock request. Token-based algo?" }
            checkOneByOne()
            checkAll()
            checkRepeated()
        }
    }

    private fun checkOneByOne() {
        // lock & unlock one-by-one
        for (i in 1..n) {
            sys.reqLock(i)
            sys.awaitLock(i)
            sys.reqUnlock(i)
            sys.awaitUnlock(i)
            val cnt = sys.awaitPingPongAndCountMessages()
            check(skipCountCheck || cnt <= 2 * (n - 1)) { "$cnt messages per lock. Lamport algo?" }
        }
    }

    private fun checkAll() {
        // request lock all (in some random order)
        val set = (1..n).shuffled(rnd).toMutableSet()
        for (i in set) {
            sys.reqLock(i)
        }
        repeat(n) {
            val k = sys.awaitAnyLock()
            check(set.remove(k))
            sys.reqUnlock(k)
            sys.awaitUnlock(k)
        }
        val cnt = sys.awaitPingPongAndCountMessages()
        check(skipCountCheck || cnt <= 2 * (n - 1) * n) { "$cnt messages per $n locks. Lamport algo?" }
    }

    private fun checkRepeated() {
        // repeated lock request by the same process
        val i = rnd.nextInt(1..n)
        repeat(3) { time ->
            sys.reqLock(i)
            sys.awaitLock(i)
            sys.reqUnlock(i)
            sys.awaitUnlock(i)
            val cnt = sys.awaitPingPongAndCountMessages()
            if (time > 0) {
                check(skipCountCheck || cnt == 0) { "$cnt messages on repeated lock. Not Philosopher's algo!" }
            }
        }
        // request lock repeatedly between this and another process
        val j = generateSequence { rnd.nextInt(1..n) }.first { it != i }
        repeat(3) { times ->
            sys.reqLock(i)
            sys.reqLock(j)
            val set = mutableSetOf(i, j)
            while (!set.isEmpty()) {
                val k = sys.awaitAnyLock()
                check(set.remove(k))
                sys.reqUnlock(k)
                sys.awaitUnlock(k)
            }
            val cnt = sys.awaitPingPongAndCountMessages()
            if (times > 0) {
                check(skipCountCheck || cnt <= 4) { "$cnt messages per two repeated locks between pair. Not Philosopher's algo" }
            }
        }
    }
}