import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.concurrent.TrieMap

object Main extends App {

  val k = readInt
  val n = readInt

  val p = for {
    i <- 0 until n
  } yield {
    val Array(x, y) = readLine.split("\\s+").map(_.toInt)
    (x, y)
  }

  val dY: Double = p.map { case (_, y) => y / n.toDouble * y }.sum

  val mpX: TrieMap[Int, Double] = TrieMap()
  val mpY: TrieMap[Int, Double] = TrieMap()

  p.foreach {
    case (x, y) => {

      val vX: Double = mpX.getOrElse(x - 1, 0.0)
      mpX.put(x - 1, vX + 1.0 / n.toDouble)

      val vY: Double = mpY.getOrElse(x - 1, 0.0)
      mpY.put(x - 1, vY + y / n.toDouble)

    }
  }

  val mp = (mpX.toList ++ mpY.toList)
  val d = mp.groupBy(_._1).map { case (k, v) =>
    v.map(_._2) match {
      case x :: y :: Nil => k -> (x, y)
      case _             => ???
    }
  }
  val eps = 1e-8

  val e = d.values
    .filter { case (x, _) => math.abs(x) > eps }
    .map { case (x, y) => y * y / x }
    .sum
  println(dY - e)
}
