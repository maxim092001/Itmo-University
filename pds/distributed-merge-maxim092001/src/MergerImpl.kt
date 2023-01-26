import system.MergerEnvironment
import kotlin.collections.ArrayList
import kotlin.collections.HashMap

class MergerImpl<T : Comparable<T>>(
    private val mergerEnvironment: MergerEnvironment<T>,
    prevStepBatches: Map<Int, List<T>>?
) : Merger<T> {

    private val batches = HashMap<Int, Int>()
    private val heap = sortedSetOf(comparator = compareBy<Pair<T, Int>> { it.first }.thenBy { it.second })
    private val stepBatches: MutableMap<Int, List<T>> = prevStepBatches?.toMutableMap() ?: HashMap()
    private val dataHoldersRange = (0 until mergerEnvironment.dataHoldersCount)

    init {
        dataHoldersRange.forEach { i ->
            batches[i] = 0
            stepBatches[i]?.takeIf { it.isNotEmpty() }?.let { heap.add(it[0] to i) }
        }
    }

    override fun mergeStep(): T? {
        fillHeap()
        return if (heap.isEmpty()) {
            null
        } else {

            val first = heap.first()
            heap.remove(first)
            batches[first.second] = (batches[first.second] ?: 0) + 1
            fillHeap()

            stepBatches[first.second]?.let { sb ->
                batches[first.second]?.takeIf { it < sb.size }?.let { b -> heap.add(sb[b] to first.second) }
            }

            first.first
        }
    }

    override fun getRemainingBatches(): Map<Int, List<T>> {
        return dataHoldersRange
            .filter { i ->
                stepBatches[i].exists { it.isEmpty() || batches[i].exists { b -> b >= it.size } }
            }
            .forEach { stepBatches.remove(it) }
            .let {
                stepBatches.map { (k, v) ->
                    k to (batches[k]?.let { b -> ArrayList(v.subList(b, v.size)) } ?: arrayListOf())
                }.toMap()
            }
    }

    private fun fillHeap() {
        dataHoldersRange
            .filter {
                !stepBatches.containsKey(it) ||
                        stepBatches[it].exists { l -> l.isEmpty() || batches[it].exists { b -> b >= l.size } }
            }
            .forEach { i ->
                mergerEnvironment.requestBatch(i).takeIf { it.isNotEmpty() }?.let { batch ->
                    stepBatches[i] = batch
                    batches[i] = 0
                    heap.add(batch[0] to i)
                }
            }
    }

    companion object {
        private fun <T> T?.exists(f: (T) -> Boolean): Boolean {
            return this?.let { f(it) } ?: false
        }
    }
}