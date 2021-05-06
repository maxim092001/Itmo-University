import java.util.Scanner
import scala.annotation.tailrec
import scala.math._

object D extends App {

  import MathOp._

  val in = new Scanner(System.in)
  val r = in.nextLong
  val k = in.nextInt

  val p: Array[Long] = Array.ofDim[Long](k + 1)
  for (i <- 0 to k) p(i) = in.nextLong

  var ans: Array[Long] = Array(0L)

  for (i <- 0 to k) {
    var t = Array(p(i) * pow(r, k - i).toLong)
    for (j <- 1 to k) t = multiplyTwoArrays(t, Array(j - i, 1L))
    ans = sum(ans, t)
  }
  val f = (1 to k).foldLeft(1L)(_ * _)
  val pw = pow(r, k).toLong
  for (i <- 0 to k) {
    val gc: Long = gcd(abs(getValOrZeroAr(ans, i)), f * pw)
    printf(s"${getValOrZeroAr(ans, i) / gc}/${f * pw / gc} ")
  }
}

object MathOp {

  @tailrec
  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a else gcd(b, a % b)
  }

  lazy val getValOrZeroAr: (Array[Long], Int) => Long = (lst, idx) => if (idx >= lst.length) 0 else lst(idx)

  def multiplyTwoArrays(f: Array[Long], s: Array[Long]): Array[Long] = {
    val newSize = s.length + f.length
    val res = Array.ofDim[Long](newSize + 1)
    for (
      i <- 0 to newSize;
      j <- 0 to i
    ) res(i) += getValOrZeroAr(f, j) * getValOrZeroAr(s, i - j)
    var degree: Int = newSize - 1
    while (degree > 0 && res(degree) == 0) degree -= 1
    res.splitAt(degree + 1)._1
  }

  def sum(f: Array[Long], s: Array[Long]): Array[Long] = {
    val newSize = max(f.length, s.length) + 1
    val ans: Array[Long] = Array.ofDim[Long](newSize)
    for (
      i <- f.indices
    ) ans(i) += f(i)
    for (
      i <- s.indices
    ) ans(i) += s(i)
    var degree: Int = newSize - 1
    while (degree > 0 && ans(degree) == 0) degree -= 1
    ans.splitAt(degree + 1)._1
  }
}