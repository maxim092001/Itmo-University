import kotlinx.atomicfu.*

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val currentSize = atomic(0)

    override fun get(index: Int): E {
        checkIndexBound(index).onFailure { throw it }
        while (true) {
            if (core.value.capacity > index) {
                return core.value.array[index].value ?: continue
            }
        }
    }

    override fun put(index: Int, element: E) {
        checkIndexBound(index).onFailure { throw it }
        while (true) {
            val coreValue = core.value
            if (coreValue.capacity > index) {
                coreValue.array[index].getAndSet(null) ?: continue
                coreValue.array[index].value = element
                return
            }
        }
    }

    override fun pushBack(element: E) {
        val newSize = currentSize.getAndIncrement()
        while (true) {
            val coreValue = core.value
            if (newSize < coreValue.capacity) {
                if (coreValue.array[newSize].compareAndSet(null, element)) return
            } else resize(coreValue)
        }
    }

    private fun resize(oldCore: Core<E>) {
        val newCore = Core<E>(oldCore.capacity + oldCore.capacity)

        if (oldCore.nxt.compareAndSet(null, newCore)) {
            for (
            i in 0 until oldCore.capacity
            ) {
                while (true) {
                    val el = oldCore.array[i].getAndSet(null)
                    if (el != null) {
                        newCore.array[i].value = el
                        break
                    }
                }
            }
            this.core.value = newCore
        }
    }

    override val size get() = currentSize.value

    private fun checkIndexBound(index: Int): Result<Unit> =
        if (index >= size || index < 0) Result.failure(IllegalArgumentException("Index out of bound exception"))
        else Result.success(Unit)

}

private class Core<E>(
    val capacity: Int,
) {
    val array = atomicArrayOfNulls<E>(capacity)
    val nxt = atomic<Core<E>?>(null)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME