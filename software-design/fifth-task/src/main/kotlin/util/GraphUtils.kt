package util

import data.Edge
import exception.Exceptions
import graph.EdgesListGraph
import graph.MatrixGraph
import java.io.File
import java.lang.Exception

object GraphUtils {

    fun readListGraph(fileName: String): EdgesListGraph {
        try {
            File(fileName).bufferedReader().use { reader ->
                val n = reader.readLine().toInt()
                val m = reader.readLine().toInt()

                val edges = (0 until m).map {
                    val e = reader.readLine().split(" ").map { it.toInt() }
                    require(e.size == 2)
                    Edge(e[0], e[1])
                }

                return EdgesListGraph(n, edges, null)
            }
        } catch (e: Exception) {
            throw Exceptions.InvalidGraphFormatException(
                """
                    Invalid list graph format.
                    Example:
                    3
                    3
                    0 1
                    1 2
                    2 0
                """.trimIndent()
            )
        }
    }

    fun readMatrixGraph(filename: String): MatrixGraph {
        try {
            File(filename).bufferedReader().use { reader ->
                val n = reader.readLine().toInt()
                val matrix: MutableList<List<Boolean>> = mutableListOf()

                (0 until n).map {
                    val l = reader.readLine().split(" ").map {
                        when (val x = it.toInt()) {
                            0 -> false
                            1 -> true
                            else -> throw IllegalArgumentException("Unexpected character $x")
                        }
                    }

                    require(l.size == n)
                    matrix.add(l)
                }

                return MatrixGraph(matrix, null)
            }
        } catch (e: Exception) {
            throw Exceptions.InvalidGraphFormatException(
                """
                    Invalid matrix graph format.
                    Example:
                    3
                    0 1 1
                    1 0 1
                    1 1 0
                """.trimIndent()
            )
        }
    }
}