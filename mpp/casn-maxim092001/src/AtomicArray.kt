import kotlinx.atomicfu.*

sealed interface CellStatus
object Unknown : CellStatus
object Success : CellStatus
object Failure : CellStatus


interface Descriptor<E> {
    fun complete()

    fun doubleCompareSingleSwap(
        ref1: Ref<E>, expected1: E, update1: Any?,
        otherDescriptor: CAS2Descriptor<E>
    ): Boolean {
        val descriptor = RDCSSDescriptor(ref1, expected1, update1, otherDescriptor)
        return ref1.v.value?.equals(update1) ?: false || if (ref1.cas(expected1, descriptor)) {
            descriptor.complete()
            descriptor.cellStatus.value == Success
        } else {
            false
        }
    }

}

class Ref<E>(initialValue: E) {
    val v = atomic<Any?>(initialValue)

    @Suppress("UNCHECKED_CAST")
    var value: E
        get() {
            while (true) {
                when (val cur = v.value) {
                    is Descriptor<*> -> cur.complete()
                    else -> return cur as E
                }
            }
        }
        set(value) {
            while (true) {
                when (val cur = v.value) {
                    is Descriptor<*> -> cur.complete()
                    else -> if (v.compareAndSet(cur, value)) return
                }
            }
        }

    fun cas(expected: Any?, update: Any?): Boolean {
        while (true) {
            when (val cur = v.value) {
                is Descriptor<*> -> cur.complete()
                expected -> {
                    val res = v.compareAndSet(cur, update)
                    if (res) {
                        return true
                    }
                }
                else -> return false
            }
        }
    }
}

class RDCSSDescriptor<E>(
    private val ref1: Ref<E>, private val expected1: E, private val update1: Any?,
    private val otherDescriptor: CAS2Descriptor<E>
) : Descriptor<E> {
    val cellStatus: AtomicRef<CellStatus> = atomic(Unknown)

    override fun complete() {
        val out = when (otherDescriptor.cellStatus.value) {
            Unknown -> Success
            else -> Failure
        }
        cellStatus.compareAndSet(Unknown, out)
        val update = when {
            cellStatus.value === Success -> update1
            else -> expected1
        }
        ref1.v.compareAndSet(this, update)
    }
}

class CAS2Descriptor<E>(
    private val ref1: Ref<E>, private val expected1: E, private val update1: E,
    private val ref2: Ref<E>, private val expected2: E, private val update2: E
) : Descriptor<E> {
    val cellStatus: AtomicRef<CellStatus> = atomic(Unknown)

    override fun complete() {
        when {
            doubleCompareSingleSwap(ref2, expected2, this, this) -> this.cellStatus.compareAndSet(Unknown, Success)
            else -> {
                val outcome = if (ref2.v.value != this) Failure else Success
                this.cellStatus.compareAndSet(Unknown, outcome)
            }
        }

        val (first, second) = when (this.cellStatus.value) {
            Failure -> Pair(expected1, expected2)
            else -> Pair(update1, update2)
        }
        ref1.v.compareAndSet(this, first)
        ref2.v.compareAndSet(this, second)
    }
}

class AtomicArray<E>(size: Int, initialValue: E) {
    private val a = arrayOfNulls<Ref<E>>(size)

    init {
        (0 until size).forEach { i ->
            a[i] = Ref(initialValue)
        }
    }

    fun get(index: Int): E = a[index]!!.value

    fun set(index: Int, value: E) {
        a[index]?.value = value
    }

    fun cas(index: Int, expected: E, update: E): Boolean = a[index]?.cas(expected, update) ?: false

    fun cas2(
        index1: Int, expected1: E, update1: E,
        index2: Int, expected2: E, update2: E
    ): Boolean {

        fun getValues(): Triple<Int, E, CAS2Descriptor<E>> = when {
                index1 > index2 ->
                    Triple(index2, expected2, CAS2Descriptor(a[index2]!!, expected2, update2, a[index1]!!, expected1, update1))
                else ->
                    Triple(index1, expected1, CAS2Descriptor(a[index1]!!, expected1, update1, a[index2]!!, expected2, update2))
            }

        return if (index1 == index2) {
            if (expected1 == expected2) cas(index1, expected1, update2) else false
        } else {
            val (index, expected, descriptor) = getValues()
            if (a[index]!!.cas(expected, descriptor)) {
                descriptor.complete()
                descriptor.cellStatus.value == Success
            } else {
                false
            }
        }
    }
}