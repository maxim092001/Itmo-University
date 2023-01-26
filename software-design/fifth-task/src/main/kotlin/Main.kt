import app.AwtApp
import app.JavaFxApp
import exception.Exceptions
import graph.Graph

import kotlinx.cli.*
import util.GraphUtils.readListGraph
import util.GraphUtils.readMatrixGraph
import java.time.LocalDate
import kotlin.properties.Delegates

fun main(args: Array<String>) {
    val parser = ArgParser("graph-app")

    val graphType by parser.option(
        ArgType.Choice<GraphType>(),
        shortName = "g",
        description = "Graph type"
    ).required()

    val drawingApi by parser.option(
        ArgType.Choice<DrawingApiType>(),
        shortName = "d",
        description = "Drawing api"
    ).required()

    val input by parser.option(
        ArgType.String,
        shortName = "i",
        description = "Input file with graph of given type"
    ).required()

    val width: Int by parser.option(
        ArgType.Int,
        shortName = "wd",
        description = "Window width"
    ).default(600)

    val height: Int by parser.option(
        ArgType.Int,
        shortName = "ht",
        description = "Window height"
    ).default(400)

    parser.parse(args)

    Globals.height = height.toLong()
    Globals.width = width.toLong()
    try {
        Globals.graph = when (graphType) {
            GraphType.List -> readListGraph(input)
            GraphType.Matrix -> readMatrixGraph(input)
        }
    } catch (e: Exceptions.InvalidGraphFormatException) {
        println(e.message)
        return
    }

    val app = when (drawingApi) {
        DrawingApiType.Awt -> AwtApp()
        DrawingApiType.JavaFx -> JavaFxApp()
    }

    app.visualizeGraph()
}

enum class GraphType {
    Matrix,
    List
}

enum class DrawingApiType {
    JavaFx,
    Awt
}

object Globals {
    lateinit var graph: Graph
    var width: Long by Delegates.notNull()
    var height: Long by Delegates.notNull()
}