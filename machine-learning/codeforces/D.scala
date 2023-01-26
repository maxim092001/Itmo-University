import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._

object A extends App {
  val Array(n, m) = readLine.split(" ").map(_.toInt)

  val objects = for {
    _ <- 0 until n
    arr = readLine.split(" ").map(_.toDouble).toIndexedSeq
  } yield (arr.dropRight(1), arr.last)

  val coefficients = readLine.split(" ").map(_.toDouble).toIndexedSeq

  def linearF(c: IndexedSeq[Double]): IndexedSeq[Double] => Double = x =>
    c.last + x.zip(c).map { case (x, y) => x * y }.sum

  def dsMape(x: IndexedSeq[Double], yt: Double): IndexedSeq[Double] = {
    val y = linearF(coefficients)(x)
    val l = (y - yt).signum * (abs(y) + abs(yt))
    val r = (y).signum * abs(y - yt)
    val b = (abs(y) + abs(yt)) * (abs(y) + abs(yt))
    val c: Double = (l - r) / b.toDouble
    val res = x.map(_ * c)
    res ++ Seq(c)
  }


  for {
    (x, y) <- objects
  } yield {
    dsMape(x, y).map(e => print(s"$e "))
    println()
  }

}
