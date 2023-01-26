package app

import draw.api.AwtDrawingApi
import draw.api.DrawingApi
import draw.api.JavaFxDrawingApi
import javafx.application.Application
import javafx.stage.Stage
import java.awt.Frame
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

sealed interface DrawingApp {
    fun visualizeGraph()
}

class JavaFxApp : Application(), DrawingApp {
    override fun start(primaryStage: Stage) {
        val drawingApi: DrawingApi = JavaFxDrawingApi(
            Globals.width,
            Globals.height,
            primaryStage
        )
        Globals.graph.drawingApi = drawingApi
        Globals.graph.drawGraph()
    }

    override fun visualizeGraph() {
        launch(this::class.java)
    }

}

class AwtApp : Frame(), DrawingApp {
    override fun visualizeGraph() {
        val closeListener = object : WindowAdapter() {
            override fun windowClosing(ignored: WindowEvent?) {
                dispose()
            }
        }

        addWindowListener(closeListener)
        setSize(Globals.width.toInt(), Globals.height.toInt())
        isVisible = true
    }

    override fun paint(graphics: Graphics) {
        super.paint(graphics)
        when (graphics) {
            is Graphics2D -> {
                graphics.clearRect(0, 0, Globals.width.toInt(), Globals.height.toInt())
                val drawingApi = AwtDrawingApi(
                    Globals.width,
                    Globals.height,
                    graphics
                )
                Globals.graph.drawingApi = drawingApi
                Globals.graph.drawGraph()
            }

            else -> throw IllegalStateException("Unknown awt application settings")
        }
    }
}