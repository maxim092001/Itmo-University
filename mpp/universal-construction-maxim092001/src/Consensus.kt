import kotlinx.atomicfu.*
import java.util.*

/**
 * Консенсус.
 */
class Consensus<T> {
    private val decision = atomic<Any?>(UNDECIDED)
    private val threads = Collections.synchronizedSet(HashSet<Thread>())

    /**
     * Принимает решение на основании предложение [value] от данного потока.
     * Каждый поток может использовать эту операцию не более одного раза.
     */
    fun decide(value: T): T {
        check(threads.add(Thread.currentThread())) { "Thread ${Thread.currentThread()} is using consensus object twice" }
        if (decision.compareAndSet(UNDECIDED, value)) return value
        @Suppress("UNCHECKED_CAST")
        return decision.value as T
    }

    private object UNDECIDED
}
