package dijkstra.sequential

import dijkstra.graph.Graph
import java.util.*
import kotlin.collections.HashSet

private fun getClosestNode(notVisitedNodes: TreeMap<Long, HashSet<Int>>): Pair<Int, Long> {
    val nodesWithMinDist = notVisitedNodes.firstEntry()
    val closestNodes = nodesWithMinDist.value
    val minDist = nodesWithMinDist.key
    assert(closestNodes.isNotEmpty())
    val res = closestNodes.iterator().next()
    val removeResult = closestNodes.remove(res)
    assert(removeResult)
    if (closestNodes.isEmpty()) {
        val setRemoveResult = notVisitedNodes.remove(minDist)
        assert(setRemoveResult === closestNodes)
    }
    return Pair(res, minDist)
}

private fun deleteNode(notVisitedNodes: TreeMap<Long, HashSet<Int>>, dist: Long, nodeId: Int) {
    val sameDistSet = notVisitedNodes[dist] ?: throw AssertionError()
    assert(sameDistSet.contains(nodeId))
    sameDistSet.remove(nodeId)
    if (sameDistSet.isEmpty()) {
        notVisitedNodes.remove(dist)
    }
}

private fun addNode(notVisitedNodes: TreeMap<Long, HashSet<Int>>, dist: Long, nodeId: Int) {
    val sameDistSet = notVisitedNodes[dist]
    if (sameDistSet == null) {
        notVisitedNodes[dist] = hashSetOf(nodeId)
    } else {
        assert(!sameDistSet.contains(nodeId))
        sameDistSet.add(nodeId)
    }
}

fun dijkstraHeap(graph: Graph, sId: Int): List<Long?> {
    val result = MutableList<Long?>(graph.graph.size) { null }
    result[sId] = 0
    val notVisitedNodes = TreeMap<Long, HashSet<Int>>()
    notVisitedNodes[0] = hashSetOf(sId)

    while (notVisitedNodes.isNotEmpty()) {
        val (closestNode, minDist) = getClosestNode(notVisitedNodes)
        val curDist = result[closestNode] ?: throw AssertionError()
        assert(curDist == minDist)
        for ((curNeighbour, w) in graph.graph[closestNode]) {
            val neighbourDist = result[curNeighbour]
            if (neighbourDist == null) {
                result[curNeighbour] = curDist + w
                addNode(notVisitedNodes, curDist + w, curNeighbour)
            } else if (neighbourDist > curDist + w) {
                result[curNeighbour] = curDist + w
                deleteNode(notVisitedNodes, neighbourDist, curNeighbour)
                addNode(notVisitedNodes, curDist + w, curNeighbour)
            }
        }
    }

    return result.toList()
}