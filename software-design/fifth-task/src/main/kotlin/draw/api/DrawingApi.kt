package draw.api

import data.Circle
import data.Point
import javafx.stage.Stage
import javafx.scene.canvas.Canvas
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.canvas.GraphicsContext
import java.awt.Graphics2D
import java.awt.geom.Ellipse2D

sealed interface DrawingApi {
    val drawingAreaWidth: Long
    val drawingAreaHeight: Long

    fun drawCircle(circle: Circle)

    fun drawLine(start: Point, end: Point)

    fun showWindow()
}


class JavaFxDrawingApi(
    override val drawingAreaWidth: Long,
    override val drawingAreaHeight: Long,
    private val primaryStage: Stage
) : DrawingApi {
    private val canvas: Canvas = Canvas(
        drawingAreaWidth.toDouble(),
        drawingAreaHeight.toDouble()
    )
    private val gc: GraphicsContext = canvas.graphicsContext2D

    override fun drawCircle(circle: Circle) {
        gc.fillOval(
            circle.center.x - circle.radius,
            circle.center.y - circle.radius,
            2 * circle.radius,
            2 * circle.radius
        )
    }

    override fun drawLine(start: Point, end: Point) {
        gc.strokeLine(
            start.x,
            start.y,
            end.x,
            end.y
        )
    }

    override fun showWindow() {
        val root = Group()
        root.children.add(canvas)
        primaryStage.scene = Scene(root)
        primaryStage.show()
    }

}

class AwtDrawingApi(
    override val drawingAreaWidth: Long,
    override val drawingAreaHeight: Long,
    private val ga: Graphics2D
): DrawingApi {

    override fun drawCircle(circle: Circle) {
        ga.fill(
            Ellipse2D.Double(
                circle.center.x - circle.radius,
                circle.center.y - circle.radius,
                2 * circle.radius,
                2 * circle.radius
            )
        )
    }

    override fun drawLine(start: Point, end: Point) {
        ga.drawLine(
            start.x.toInt(),
            start.y.toInt(),
            end.x.toInt(),
            end.y.toInt()
        )
    }

    override fun showWindow() {
        // Nothing to do
    }
}