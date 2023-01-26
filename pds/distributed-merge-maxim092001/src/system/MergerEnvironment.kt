package system

interface MergerEnvironment<T : Comparable<T>> {
    val dataHoldersCount: Int

    fun requestBatch(dataHolderId: Int): List<T>
}