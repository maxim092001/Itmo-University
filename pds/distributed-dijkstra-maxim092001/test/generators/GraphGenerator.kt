package dijkstra.generators

import dijkstra.graph.Graph
import kotlin.random.Random

fun generateGraph(
    nodeFrom: Int, nodeTo: Int,
    edgesFromTo: (Int) -> Pair<Int, Int>,
    weightFrom: Long, weightTo: Long,
    skipEdges: Boolean
): Graph {
    val random = Random(System.currentTimeMillis())
    val nodes = random.nextInt(from = nodeFrom, until = nodeTo)
    val (edgesFrom, edgesTo) = edgesFromTo(nodes)
    val edges = random.nextInt(from = edgesFrom, until = edgesTo)

    var edgesAdded = 0
    val curEdges = MutableList(nodes) { mutableMapOf<Int, Long>() }
    while (edgesAdded < edges) {
        val src = random.nextInt(from = 0, until = nodes)
        val dst = random.nextInt(from = 0, until = nodes)
        val weight = random.nextLong(from = weightFrom, until = weightTo)
        if (curEdges[src].containsKey(dst)) {
            if (skipEdges) {
                edgesAdded += 1
            }
            continue
        }
        curEdges[src][dst] = weight
    }

    return Graph(
        curEdges.map { it.toMap() }.toList()
    )
}