interface Merger<T : Comparable<T>> {
    fun mergeStep(): T?

    fun getRemainingBatches(): Map<Int, List<T>>
}