package dijkstra

import org.junit.Test
import java.util.*
import kotlin.test.assertEquals

class ConcurrentStressTest {

    @Test(timeout = 1_000)
    fun `test on trees`() {
        testOnRandomGraphs(100, 99)
    }

    @Test(timeout = 1_000)
    fun `test on very small graphs`() {
        testOnRandomGraphs(16, 25)
    }

    @Test(timeout = 10_000)
    fun `test on small graphs`() {
        testOnRandomGraphs(100, 1000)
    }

    private fun testOnRandomGraphs(nodes: Int, edges: Int) {
        val r = Random(0)
        repeat(GRAPHS) {
            val nodesList = randomConnectedGraph(nodes, edges)
            repeat(SEARCHES) {
                val from = nodesList[r.nextInt(nodes)]
                shortestPathSequential(from)
                val seqRes = nodesList.map { it.distance }
                clearNodes(nodesList)
                shortestPathParallel(from)
                val parRes = nodesList.map { it.distance }
                clearNodes(nodesList)
                assertEquals(seqRes, parRes)
            }
        }
    }
}

private const val GRAPHS = 10
private const val SEARCHES = 100