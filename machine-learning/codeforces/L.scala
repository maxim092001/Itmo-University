import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.annotation.tailrec

object Main extends App {
  val n = readLine.toInt

  val (xs, ys) = (for {
    _ <- 0 until n
  } yield {
    val Array(x, y) = readLine.split(" ").map(_.toLong)
    (x, y)
  }).toList.unzip

  println(Coefficients.countSpirmen(xs, ys))
}

object Coefficients {

  private def average(l: List[Double]): Double = l.sum / l.length

  private def disp(l: List[Double]) = {
    val avg = average(l)
    l.map { el => (el - avg) * (el - avg) }.sum
  }

  private def covar(l1: List[Double], l2: List[Double]): Double = {
    val (l1Avg, l2Avg) = (average(l1), average(l2))

    l1.zip(l2).map { case (x, y) => (x - l1Avg) * (y - l2Avg) }.sum
  }

  private def ranks(l1: List[Long]): List[Long] = {

    @tailrec
    def rec(res: List[(Int, Long)], l1: List[(Long, Int)], prev: Long, cnt: Long): List[Long] = l1 match {
      case Nil => res.sortBy(_._1).map(_._2).toList
      case (el, idx) :: tail =>
        val cntL = cnt + (if (el == prev) 0 else 1)
        rec((idx -> cntL) :: res, tail, el, cntL)
    }

    l1 match {
      case Nil => Nil
      case x :: tail =>
        val zipped = l1.zipWithIndex.sortBy(_._1)
        rec(List(zipped.head._2 -> 0L), zipped.tail, x, 0)
    }

  }

  def countSpirmen(xs: List[Long], ys: List[Long]): Double = xs match {
    case Nil | (_ :: Nil) => 0
    case l =>
      val sum: Long = ranks(xs).zip(ranks(ys)).map { case (x, y) => (x - y) * (x - y) }.sum
      1.0 - 6.0 * sum / (l.length * (l.length - 1.0) * (l.length + 1.0))
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
