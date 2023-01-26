import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._

object Main extends App {
  val n = readLine.toInt

  val (xs, ys) = (for {
    _ <- 0 until n
  } yield {
    val Array(x, y) = readLine.split(" ").map(_.toDouble)
    (x, y)
  }).toList.unzip

  println(Pirson.countPirson(xs, ys))
}

object Pirson {

  private def average(l: List[Double]): Double = l.sum / l.length

  private def disp(l: List[Double]) = {
    val avg = average(l)
    l.map { el => (el - avg) * (el - avg) }.sum
  }

  private def covar(l1: List[Double], l2: List[Double]): Double = {
    val (l1Avg, l2Avg) = (average(l1), average(l2))

    l1.zip(l2).map { case (x, y) => (x - l1Avg) * (y - l2Avg) }.sum
  }

  def countPirson(xs: List[Double], ys: List[Double]): Double = {
    val (xDisp, yDisp) = (disp(xs), disp(ys))

    if (xDisp * yDisp == 0) {
      0
    } else {
      covar(xs, ys) / sqrt(xDisp * yDisp)
    }
  }
}
