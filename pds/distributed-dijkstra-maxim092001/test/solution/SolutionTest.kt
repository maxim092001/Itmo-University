package dijkstra.solution

import dijkstra.generators.generateGraph
import dijkstra.sequential.dijkstraHeap
import dijkstra.system.runtime.FIFORuntime
import dijkstra.system.executor.SystemExecutor
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import kotlin.random.Random

class SolutionTest {
    @Test
    fun stress() {
        repeat(1000) {
            if (it % 10 == 0) {
                println(it)
            }
            val graph = generateGraph(
                nodeFrom = 20, nodeTo = 50,
                edgesFromTo = { nodes -> Pair(0, nodes * nodes / 5) },
                weightFrom = 0L, weightTo = 100L,
                skipEdges = true
            )
            val executor = SystemExecutor(graph = graph) { FIFORuntime() }
            val sId = Random(System.currentTimeMillis()).nextInt(from = 0, until = graph.graph.size)
            val expectedAns = dijkstraHeap(graph, sId)
            val ans = executor.execute(sId)
            Assertions.assertEquals(expectedAns, ans)
        }
    }
}