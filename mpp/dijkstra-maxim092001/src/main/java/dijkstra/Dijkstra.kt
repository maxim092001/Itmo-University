package dijkstra

import kotlinx.atomicfu.atomic
import java.util.*
import java.util.concurrent.Phaser
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }
private val random = Random()

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiQueuePriorityQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    repeat(workers) {
        thread {
            while (true) {
                val cur: Node = synchronized(q) { q.poll() } ?: if (q.isEmpty()) break else continue
                for (edge in cur.outgoingEdges) {
                    while (true) {
                        val old = edge.to.distance
                        val new = cur.distance + edge.weight
                        if (new < old) {
                            if (edge.to.casDistance(old, new)) {
                                q.add(edge.to)
                            } else continue
                        }
                        break
                    }
                }
                q.decSize()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

fun getUniqueRandomTuple(bound: Int): Pair<Int, Int> {
    var p = 0 to 0
    while (p.first == p.second) p = random.nextInt(bound) to random.nextInt(bound)
    return p
}

private class MultiQueuePriorityQueue(workers: Int, nodeComparator: Comparator<Node>) {
    private val numberOfQueues = 4 * workers
    private val multiQueue = MutableList(numberOfQueues) { PriorityQueue(nodeComparator) }
    private val size = atomic(0)

    fun add(node: Node) {
        val idx = random.nextInt(numberOfQueues)

        size.incrementAndGet()
        val queue = multiQueue[idx]
        synchronized(queue) {
            queue.add(node)
        }
    }

    fun poll(): Node? {
        val (firstIdx, secondIdx) = getUniqueRandomTuple(numberOfQueues)
        val firstQueue = multiQueue[firstIdx]
        val secondQueue = multiQueue[secondIdx]
        synchronized(firstQueue) {
            synchronized(secondQueue) {
                val isFirstEmpty = firstQueue.isEmpty()
                val isSecondEmpty = secondQueue.isEmpty()

                return when (Pair(isFirstEmpty, isSecondEmpty)) {
                    true to true -> null
                    true to false -> secondIdx
                    false to true -> firstIdx
                    else -> if (NODE_DISTANCE_COMPARATOR.compare(
                            firstQueue.peek(),
                            secondQueue.peek()
                        ) < 0
                    ) firstIdx else secondIdx
                }?.let { multiQueue[it].poll() }
            }
        }
    }

    fun isEmpty() = size.compareAndSet(0, 0)

    fun decSize() = size.decrementAndGet()

}