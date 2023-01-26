import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.concurrent.TrieMap

object Main extends App {
  val Array(k1, k2) = readLine.split("\\s+").map(_.toInt)
  val n             = readInt

  val xy = for {
    i <- 0 until n
  } yield {
    val Array(x, y) = readLine.split("\\s+").map(_.toInt)
    x -> y.toDouble
  }

  val mpX: Map[Int, Double] = xy.map(_._1).groupBy(identity).mapValues(_.size)
  val mpXY: Map[Int, Map[Double, Double]] =
    xy.groupBy(_._1).mapValues(v => v.map(_._2).groupBy(identity).mapValues(_.size))

  val mpXDiv  = mpX.mapValues(_ / n)
  val mpXYDiv = mpXY.mapValues(_.mapValues(v => v / n))

  val ans: Double = -(for {
    p1 <- mpXYDiv
    p2 <- p1._2
  } yield {
    val v = p2._2
    v * (log(v) - log(mpXDiv(p1._1)))
  }).sum
  println(ans)
}
