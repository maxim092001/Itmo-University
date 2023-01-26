import system.DataHolderEnvironment

class DataHolderImpl<T : Comparable<T>>(
    private val keys: List<T>,
    private val dataHolderEnvironment: DataHolderEnvironment
) : DataHolder<T> {

    private var checkpointIndex = 0
    private var idx = 0

    override fun checkpoint() {
        checkpointIndex = idx
    }

    override fun rollBack() {
        idx = checkpointIndex
    }

    override fun getBatch(): List<T> {
        val batchSize = dataHolderEnvironment.batchSize
        val (left, right, newValue) = if (idx + batchSize > keys.size) {
            Triple(idx, keys.size, keys.size)
        } else {
            Triple(idx, idx + batchSize, idx + batchSize)
        }
        return getBatchWith(left, right, newValue)
    }

    private fun getBatchWith(left: Int, right: Int, newValue: Int): ArrayList<T> =
        ArrayList(keys.subList(left, right)).apply {
            idx = newValue
        }
}