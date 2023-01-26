import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable

object Main extends App {
  val Array(k1, k2) = readLine.split(" ").map(_.toInt)
  val n             = readLine.toInt

  val countX = Array.ofDim[Long](k1)
  val countY = Array.ofDim[Long](k2)

  val xy: immutable.IndexedSeq[(Int, Int)] = for {
    _ <- 0 until n
  } yield {
    val Array(x, y) = readLine.split(" ").map(el => el.toInt - 1)
    countX(x) = countX(x) + 1
    countY(y) = countY(y) + 1
    (x, y)
  }
  val c: Map[Int, Map[Int, Int]] = xy.groupBy(_._1).mapValues(_.map(_._2).groupBy(identity).mapValues(_.size))

  val scaledCountX = countX.map(x => (1.0 * x) / n)
  val scaledCountY = countY.map(y => (1.0 * y) / n)

  val ans = n + (for {
    (x, yM) <- c
    (y, v)  <- yM
  } yield {
    val e: Double = scaledCountX(x) * scaledCountY(y) * n
    val d: Double = v - e
    val c: Double = d * d / e
    c - e
  }).sum

  println(ans)
}
