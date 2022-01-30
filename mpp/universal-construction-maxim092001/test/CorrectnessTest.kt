import org.junit.Test
import java.lang.reflect.*
import kotlin.concurrent.*
import kotlin.random.*
import kotlin.test.*

class CorrectnessTest {
    @Test
    fun testSimple() {
        val c = Solution()
        assertEquals(0, c.getAndAdd(1))
        assertEquals(1, c.getAndAdd(1))
        assertEquals(2, c.getAndAdd(2))
        assertEquals(4, c.getAndAdd(3))
        assertEquals(7, c.getAndAdd(-10))
        assertEquals(-3, c.getAndAdd(0))
        assertEquals(-3, c.getAndAdd(0))
    }

    @Test(timeout = 3_000)
    fun testRandom() {
        verifyRandom(1_000_000)
    }

    private fun verifyRandom(n: Int) {
        val c = Solution()
        val r = Random(1)
        var expected = 0
        repeat(n) {
            val x = r.nextInt()
            assertEquals(expected, c.getAndAdd(x))
            expected += x
        }
    }

    @Test(timeout = 10_000)
    fun testConcurrentRandom() {
        val threads = List(5) {
            thread(start = false) {
                verifyRandom(100_000)
            }
        }
        var failed = false
        threads.forEach {
            it.setUncaughtExceptionHandler { t, e ->
                println("$t: Failed with $e")
                e.printStackTrace()
                failed = true
            }
        }
        threads.forEach { it.start() }
        threads.forEach { it.join() }
        assertFalse(failed)
    }

    @Test
    fun testSolutionClass() {
        check(AtomicCounter::class.java.isAssignableFrom(Solution::class.java))
        validateClass(Solution::class.java, HashSet())
    }

    private fun validateClass(clazz: Class<*>, visited: HashSet<Class<*>>) {
        if (!visited.add(clazz)) return
        check(clazz.name == "Solution" || clazz.name.startsWith("Solution$")) { "$clazz: Must be 'Solution' or its inner class" }
        for (f in clazz.declaredFields) {
            check(!Modifier.isStatic(f.modifiers)) { "$clazz: Cannot use static fields" }
            check(Modifier.isFinal(f.modifiers)) { "$clazz: All fields must be final" }
            when (f.type) {
                ThreadLocal::class.java, Consensus::class.java, Int::class.java -> { /* ok */ }
                else -> validateClass(f.type, visited)
            }
        }
        if (clazz.superclass != Any::class.java) validateClass(clazz.superclass, visited)
    }
}