package graph

import data.Circle
import data.Edge
import data.Point
import draw.api.DrawingApi

sealed class Graph(
    protected val verticesCount: Int,
    var drawingApi: DrawingApi?
) {
    private val center: Point by lazy {
        Point(
            drawingApi!!.drawingAreaWidth / 2.0,
            drawingApi!!.drawingAreaHeight / 2.0
        )
    }

    private val radius: Double by lazy {
        kotlin.math.min(
            drawingApi!!.drawingAreaWidth,
            drawingApi!!.drawingAreaHeight
        ) / 2.0
    }

    private val verticesCoords: List<Point> by lazy {
        (0 until verticesCount)
            .map {
                Point(
                    (center.x + radius * kotlin.math.cos(2 * kotlin.math.PI * it / verticesCount)),
                    (center.y + radius * kotlin.math.sin(2 * kotlin.math.PI * it / verticesCount))
                )
            }
    }


    protected fun drawEdge(edge: Edge) {
        drawingApi!!.drawLine(verticesCoords[edge.from], verticesCoords[edge.to])
    }

    protected fun drawVertex(v: Int) {
        drawingApi!!.drawCircle(Circle(verticesCoords[v], 10.0))
    }

    abstract fun drawGraph()
}


class EdgesListGraph(
    verticesCount: Int,
    private val edges: List<Edge>,
    drawingApi: DrawingApi?
) : Graph(verticesCount, drawingApi) {

    override fun drawGraph() {
        (0 until verticesCount).forEach { drawVertex(it) }
        edges.forEach { drawEdge(it) }
        drawingApi!!.showWindow()
    }

}

class MatrixGraph(
    private val matrix: List<List<Boolean>>,
    drawingApi: DrawingApi?
) : Graph(matrix.size, drawingApi) {
    override fun drawGraph() {
        matrix.indices.forEach { drawVertex(it) }
        for (i in matrix.indices) {
            for (j in matrix.indices) {
                if (matrix[i][j]) {
                    drawEdge(Edge(i, j))
                }
            }
        }
        drawingApi!!.showWindow()
    }


}