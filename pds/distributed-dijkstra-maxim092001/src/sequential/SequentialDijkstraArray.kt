package dijkstra.sequential

import dijkstra.graph.Graph

fun dijkstraArray(graph: Graph, sId: Int): List<Long?> {
    val result = MutableList<Long?>(graph.graph.size) { null }
    val used = MutableList(graph.graph.size) { false }
    result[sId] = 0

    for (unused in graph.graph.indices) {
        var bestNode: Int? = null

        for (i in graph.graph.indices) {
            if (used[i]) {
                continue
            }
            val curDist = result[i] ?: continue
            if (bestNode == null) {
                bestNode = i
            } else {
                val bestDist = result[bestNode] ?: throw AssertionError()
                if (curDist < bestDist) {
                    bestNode = i
                }
            }
        }

        if (bestNode == null) {
            break
        }
        val bestDist = result[bestNode] ?: throw AssertionError()
        used[bestNode] = true
        for ((curNeighbour, w) in graph.graph[bestNode]) {
            val curDist = result[curNeighbour]
            if (curDist == null || bestDist + w < curDist) {
                assert(!used[curNeighbour])
                result[curNeighbour] = bestDist + w
            }
        }
    }

    return result.toList()
}