import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._
import scala.math._

object Main extends App {

  import utils._

  val Array(n, m) = readLine.split("\\s+").map(_.toInt)

  val lst = (for {
    _ <- 0 until n
  } yield {
    val l = readLine.split("\\s+").map(_.toDouble).toList
    (l.init, l.last)
  }).toList

  val targetObj: List[Double] = readLine.split("\\s+").map(_.toDouble).toList

  val (dst, kern, window, h): (String, String, String, Int) = (readLine, readLine, readLine, readInt)

  val triplets = lst
    .map { case (xs, y) =>
      (xs, y, distance(dst, targetObj, xs))
    }
    .sortBy(_._3)

  val newH: Double = Option(h.toDouble).filter(_ => window == "fixed").getOrElse(triplets(h)._3)

  if (newH == 0) {

    val ne: List[Double] = (if (triplets.head._1.equals(targetObj))
                              triplets.filter(_._1.equals(targetObj))
                            else
                              triplets).map(_._2)
    println(ne.sum / ne.length)
  } else {

    val b: Double = triplets.map(el => kernel(kern, el._3 / newH)).sum

    println(b match {
      case 0           => triplets.map(_._2).sum / n
      case denominator => triplets.map(el => el._2 * kernel(kern, el._3 / newH)).sum / denominator
    })
  }

}

object utils {

  import scala.math._

  type DistanceF = (Double, Double) => Double

  private def euclidean: DistanceF = (a, b) => (a - b) * (a - b)
  private def classic: DistanceF   = (a, b) => abs(a - b)

  private def monoF: List[Double] => List[Double] => DistanceF => (List[Double] => Double) => Double = l1 =>
    l2 => f => g => g(l1.zip(l2).map(f.tupled))
  private def sum: List[Double] => List[Double] => DistanceF => Double = l1 => l2 => f => monoF(l1)(l2)(f)(_.sum)
  private def max: List[Double] => List[Double] => DistanceF => Double = l1 => l2 => f => monoF(l1)(l2)(f)(_.max)

  def distance(name: String, x: List[Double], y: List[Double]): Double = {
    name match {
      case "euclidean" => sqrt(sum(x)(y)(euclidean))
      case "manhattan" => sum(x)(y)(classic)
      case "chebyshev" => max(x)(y)(classic)
    }
  }

  def finite(u: Double) = abs(u) < 1

  def kernel(name: String, u: Double): Double = {
    name match {
      case "epanechnikov" => if (finite(u)) 0.75 * (1 - (u * u)) else 0
      case "quartic"      => if (finite(u)) (15d / 16d) * pow(1 - u * u, 2) else 0
      case "triweight"    => if (finite(u)) (35d / 32d) * pow(1 - u * u, 3) else 0
      case "tricube"      => if (finite(u)) (70d / 81d) * pow(1 - pow(u, 3), 3) else 0
      case "uniform"      => if (finite(u)) 0.5 else 0
      case "triangular"   => if (finite(u)) 1 - u else 0
      case "gaussian"     => exp(-0.5 * u * u) / sqrt(2 * Pi)
      case "cosine"       => if (finite(u)) (Pi / 4) * cos(Pi * u / 2) else 0
      case "logistic"     => 1d / (exp(u) + 2 + exp(-u))
      case "sigmoid"      => (2d / Pi) / (exp(u) + exp(-u))
    }
  }

}
