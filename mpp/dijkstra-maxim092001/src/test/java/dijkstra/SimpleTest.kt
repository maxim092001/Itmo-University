package dijkstra

import org.junit.Test
import kotlin.test.assertEquals

class SimpleTest {

    @Test(timeout = 1_000)
    fun `Dijkstra on a small graph`() {
        val a = Node()
        val b = Node()
        val c = Node()
        val d = Node()
        val e = Node()
        a.addEdge(Edge(b, 2))
        a.addEdge(Edge(d, 1))
        b.addEdge(Edge(c, 4))
        b.addEdge(Edge(e, 5))
        c.addEdge(Edge(e, 1))
        d.addEdge(Edge(c, 3))
        val nodes = listOf(a, b, c, d, e)

        shortestPathSequential(a)
        assertEquals(0, a.distance)
        assertEquals(2, b.distance)
        assertEquals(4, c.distance)
        assertEquals(1, d.distance)
        assertEquals(5, e.distance)
        clearNodes(nodes)

        shortestPathParallel(a)
        assertEquals(0, a.distance)
        assertEquals(2, b.distance)
        assertEquals(4, c.distance)
        assertEquals(1, d.distance)
        assertEquals(5, e.distance)
        clearNodes(nodes)
    }
}