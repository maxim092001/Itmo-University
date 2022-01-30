import org.junit.*
import java.util.concurrent.atomic.*

open class TestBase {
    val mainThread = Thread.currentThread()
    val error = AtomicReference<Throwable?>(null)
    val cs = AtomicReference<Thread?>(null)

    @After
    fun tearDown() {
        val e = error.get()
        if (e != null) throw e
    }

    fun List<Thread>.configureExceptionHandlers() {
        forEach {
            it.setUncaughtExceptionHandler { t, e ->
                println("Exception in thread $t: $e")
                e.printStackTrace()
                error.compareAndSet(null, e)
                mainThread.interrupt()
            }
        }
    }

    inline fun Lock<*>.checkLock(block: () -> Unit) {
        withLock {
            val old = cs.getAndSet(Thread.currentThread())
            require(old == null) { "At most one thread can be in critical section, but $old is already there" }
            block()
            cs.set(null)
        }
    }
}