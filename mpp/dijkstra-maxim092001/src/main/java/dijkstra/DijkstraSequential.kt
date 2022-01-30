package dijkstra

import java.util.*

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathSequential(start: Node) {
    start.distance = 0
    val q = PriorityQueue<Node>(NODE_DISTANCE_COMPARATOR)
    q.add(start)
    while (q.isNotEmpty()) {
        val cur = q.poll()
        for (e in cur.outgoingEdges) {
            if (e.to.distance > cur.distance + e.weight) {
                e.to.distance = cur.distance + e.weight
                q.remove(e.to) // inefficient, but used for tests only
                q.add(e.to)
            }
        }
    }
}