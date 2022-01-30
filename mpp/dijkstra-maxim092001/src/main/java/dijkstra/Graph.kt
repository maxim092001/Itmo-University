package dijkstra

import kotlinx.atomicfu.*
import java.util.*
import kotlin.collections.ArrayList

class Node {
    private val _outgoingEdges = arrayListOf<Edge>()
    val outgoingEdges: List<Edge> = _outgoingEdges

    private val _distance = atomic(Integer.MAX_VALUE)
    var distance // USE ME FOR THE DIJKSTRA ALGORITHM!
        get() = _distance.value
        set(value) { _distance.value = value }
    fun casDistance(cur: Int, update: Int) = _distance.compareAndSet(cur, update)

    fun addEdge(edge: Edge) {
        _outgoingEdges.add(edge)
    }
}

fun clearNodes(nodes: List<Node>) {
    nodes.forEach { it.distance = Int.MAX_VALUE }
}

data class Edge(val to: Node, val weight: Int)

fun randomConnectedGraph(nodes: Int, edges: Int, maxWeight: Int = 100): List<Node> {
    require(edges >= nodes - 1)
    val r = Random()
    val nodesList = List(nodes) { Node() }
    // generate a random connected graph with `nodes-1` edges
    val s = ArrayList(nodesList)
    var cur = s.removeAt(r.nextInt(s.size))
    val visited = mutableSetOf<Node>(cur)
    while (s.isNotEmpty()) {
        val neighbor = s.removeAt(r.nextInt(s.size))
        if (visited.add(neighbor)) {
            cur.addEdge(Edge(neighbor, r.nextInt(maxWeight)))
        }
        cur = neighbor
    }
    // add `edges - nodes + 1` random edges
    repeat(edges - nodes + 1) {
        while (true) {
            val first = nodesList[r.nextInt(nodes)]
            val second = nodesList[r.nextInt(nodes)]
            if (first == second) continue
            if (first.outgoingEdges.any { e -> e.to == second }) continue
            val weight = r.nextInt(maxWeight)
            first.addEdge(Edge(second, weight))
            second.addEdge(Edge(first, weight))
            break
        }
    }
    return nodesList
}