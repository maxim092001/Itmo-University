interface DataHolder<T : Comparable<T>> {
    fun checkpoint()

    fun rollBack()

    fun getBatch(): List<T>
}