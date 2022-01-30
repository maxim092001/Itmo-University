import kotlinx.atomicfu.*

sealed interface Maybe<T>

class Done<T> : Maybe<T>
class Boxed<T>(val t: T) : Maybe<T>

class FAAQueue<T> {
    private val head: AtomicRef<Segment<T>> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment<T>> // Tail pointer, similarly to the Michael-Scott queue

    init {
        val firstNode = Segment<T>()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: T) {
        while (true) {
            val tailVal = tail.value
            val tailNextVal = tailVal.next.value
            if (tailNextVal != null) {
                tail.compareAndSet(tailVal, tailNextVal)
                continue
            }
            val enqIdx = tailVal.enqIdx.getAndIncrement()
            if (enqIdx >= SEGMENT_SIZE) {
                if (tail.value.next.compareAndSet(null, Segment(x))) {
                    return
                }
            } else {
                if (tailVal.elements[enqIdx].compareAndSet(null, Boxed(x))) {
                    return
                }
            }
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): T? {
        while (true) {
            val headVal = head.value
            val deqIdx = headVal.deqIdx.getAndIncrement()
            if (deqIdx >= SEGMENT_SIZE) {
                if (headVal.next.value != null) {
                    val headNext = headVal.next.value!!
                    head.compareAndSet(headVal, headNext)
                    continue
                }
                return null
            }
            return when (val elem = headVal.elements[deqIdx].getAndSet(Done())) {
                is Boxed -> elem.t
                is Done -> null
                null -> continue
            }
        }
    }

    /**
     * Returns `true` if this queue is empty;
     * `false` otherwise.
     */
    val isEmpty: Boolean
        get() {
            while (true) {
                return if (head.value.isEmpty) {
                    if (head.value.next.value != null) {
                        head.value = head.value.next.value!!
                        continue
                    }
                    true
                } else {
                    false
                }
            }
        }
}

private class Segment<T> {
    val next = atomic<Segment<T>?>(null)
    val enqIdx = atomic(0) // index for the next enqueue operation
    val deqIdx = atomic(0) // index for the next dequeue operation
    val elements = atomicArrayOfNulls<Maybe<T?>>(SEGMENT_SIZE)

    constructor() // for the first segment creation

    constructor(x: T?) { // each next new segment should be constructed with an element
        elements[0].getAndSet(Boxed(x))
        enqIdx.incrementAndGet()
    }

    val isEmpty: Boolean
        get() {
            val deqValue = deqIdx.value
            val enqValue = enqIdx.value
            return deqValue >= enqValue || deqValue >= SEGMENT_SIZE
        }

}

const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS

