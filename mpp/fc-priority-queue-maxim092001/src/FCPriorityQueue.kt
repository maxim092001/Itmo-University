import kotlinx.atomicfu.atomicArrayOfNulls
import kotlinx.atomicfu.locks.ReentrantLock
import java.util.*

sealed interface OperationName
object Add : OperationName
object Peek : OperationName
object Poll : OperationName
object Done : OperationName

data class Operation<E>(var op: OperationName, var element: E? = null)

class FCPriorityQueue<E : Comparable<E>> {
    private val lock = ReentrantLock()
    private val rnd = Random()
    private val numberOfWorkers = 4 * Runtime.getRuntime().availableProcessors()
    private val operations = atomicArrayOfNulls<Operation<E>?>(numberOfWorkers)
    private val queue = PriorityQueue<E>()

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? = operation(Operation(Poll), retrieveElement)

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? = operation(Operation(Peek), retrieveElement)

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) = operation(Operation(Add, element)) { idx, _ -> operations[idx].value = null }

    private val retrieveElement: (Int, Operation<E>) -> E? = { idx: Int, worker: Operation<E> ->
        val res = worker.element
        operations[idx].value = null
        res
    }

    private fun <R> operation(operation: Operation<E>, ret: (Int, Operation<E>) -> R): R {
        while (true) {
            val randomIndex = rnd.nextInt(numberOfWorkers)
            if (operations[randomIndex].compareAndSet(null, operation)) {
                if (operations[randomIndex].value != operation) throw IllegalArgumentException("Wrong argument")
                while (true) {
                    if (lock.tryLock()) {
                        for (i in 0 until numberOfWorkers) {
                            val currentOperation = operations[i].value ?: continue
                            when (currentOperation.op) {
                                Add -> queue.add(currentOperation.element)
                                Poll -> currentOperation.element = queue.poll()
                                Peek -> currentOperation.element = queue.peek()
                                Done -> {}
                            }
                            currentOperation.op = Done
                        }
                        lock.unlock()
                    }
                    if (operation.op == Done) {
                        return ret(randomIndex, operation)
                    }
                }
            }
        }
    }
}
