import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object A extends App {
  val k: Int = StdIn.readInt
  
  val arr: IndexedSeq[IndexedSeq[Double]] = (for (
    _ <- 0 until k
  ) yield StdIn.readLine().split(" ").map(_.toDouble).toIndexedSeq)

  val s = arr.map(_.sum).sum
   
  val p = for {
    i <- 0 until k
    rowSum = arr(i).sum
    colSum = (for (j <- 0 until k) yield arr(j)(i)).sum
  } yield {
    if (colSum != 0) arr(i)(i) / colSum else 0
  }
   
  val r = for {
    i <- 0 until k
    rowSum = arr(i).sum
    colSum = (for (j <- 0 until k) yield arr(j)(i)).sum
  } yield {
    if (rowSum != 0) arr(i)(i) / rowSum else 0
  }


  val f = for {
    i <- 0 until k
    rowSum = arr(i).sum
    colSum = (for (j <- 0 until k) yield arr(j)(i)).sum
  } yield {
    if (p(i) + r(i) == 0) 0 else 2 * p(i) * r(i) / (p(i) + r(i))
  }

  val fw = (for {
    i <- 0 until k
    rowSum = arr(i).sum
  } yield {
   rowSum * f(i) 
  }).sum


  val pw = (for {
    i <- 0 until k
    rowSum = arr(i).sum
  } yield {
   rowSum * p(i) 
  }).sum


  val rw = (for {
    i <- 0 until k
    rowSum = arr(i).sum
  } yield {
   rowSum * r(i) 
  }).sum

  val tp = (for {
    i <- 0 until k
    rowSum = arr(i).sum
  } yield {
   rowSum * arr(i)(i) 
  }).sum

  val fp = (for {
    i <- 0 until k
    rowSum = arr(i).sum
    colSum = (for (j <- 0 until k) yield arr(j)(i)).sum
  } yield {
   rowSum * (colSum - arr(i)(i)) 
  }).sum
  
  
  val fn = (for {
    i <- 0 until k
    rowSum = arr(i).sum
  } yield {
   rowSum * (rowSum  - arr(i)(i)) 
  }).sum
 
  val pwD = pw / s
  val rwD = rw / s
  val fRegD = fw / s
  val tpD = tp / s
  val fpD = fp / s
  val fnD = fn / s

  val fMicro = if (tpD + 0.5 * (fpD + fnD) == 0) 0 else tpD / (tpD + 0.5 * (fpD + fnD))
  val fMacro = if (rw + pw == 0) 0 else (2 * pwD * rwD / (pwD + rwD))

  println(fMicro)
  println(fMacro)
  println(fRegD)
}
