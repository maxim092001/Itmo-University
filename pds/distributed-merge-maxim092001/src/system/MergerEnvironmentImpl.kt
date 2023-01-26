package system

import DataHolder

class MergerEnvironmentImpl<T : Comparable<T>>(
    private val dataHolders: List<DataHolder<T>>,
    private val dataHolderEnvironment: DataHolderEnvironment
) : MergerEnvironment<T> {
    companion object {
        enum class DataHolderStatus {
            FINISHED,
            FINISHED_CHECKPOINT_PASSED,
            NOT_FINISHED
        }
    }

    override val dataHoldersCount: Int = dataHolders.size

    private val finished = Array(dataHoldersCount) { DataHolderStatus.NOT_FINISHED }

    private var keysCount = -1

    val curKeysCount: Int
        get() = keysCount

    fun rollback() {
        for (i in 0 until dataHoldersCount) {
            if (finished[i] == DataHolderStatus.FINISHED) {
                finished[i] = DataHolderStatus.NOT_FINISHED
            }
        }
    }

    fun checkpoint() {
        for (i in 0 until dataHoldersCount) {
            if (finished[i] == DataHolderStatus.FINISHED) {
                finished[i] = DataHolderStatus.FINISHED_CHECKPOINT_PASSED
            }
        }
    }

    fun newMergerStarted(prevStepBatches: Map<Int, List<T>>?) {
        assert(
            prevStepBatches == null && keysCount == -1 ||
                    0 <= keysCount && keysCount <= dataHolderEnvironment.batchSize * dataHoldersCount
        )
        keysCount = prevStepBatches?.map { it.value.size }?.sum() ?: 0
    }

    fun keyYielded() {
        assert(1 <= keysCount && keysCount - 1 <= dataHolderEnvironment.batchSize * dataHoldersCount)
        keysCount -= 1
    }

    override fun requestBatch(dataHolderId: Int): List<T> {
        assert(0 <= keysCount && keysCount <= dataHolderEnvironment.batchSize * dataHoldersCount)
        val res = dataHolders[dataHolderId].getBatch()
        if (res.size > dataHolderEnvironment.batchSize) {
            throw IllegalStateException("Batch size is greater, than required")
        }
        if (res.size < dataHolderEnvironment.batchSize) {
            if (res.isNotEmpty() && finished[dataHolderId] != DataHolderStatus.NOT_FINISHED) {
                throw IllegalStateException("One of the returned batches has size less, than required")
            }
            finished[dataHolderId] = DataHolderStatus.FINISHED
        }

        if (keysCount + res.size - 1 > dataHolderEnvironment.batchSize * dataHoldersCount) {
            throw IllegalStateException("Too many keys requested")
        }
        keysCount += res.size

        return res
    }
}