
import java.util.Scanner
import scala.math.min

object B extends App {


  val in: Scanner = new Scanner(System.in)
  val n = in.nextInt()
  val m = in.nextInt()
  val a = Array.ofDim[Long](n + 1)
  for (i <- 0 to n) {
    a(i) = in.nextLong()
  }
  val ops = MathOperations(a, m)
  ops.sqrt
  ops.exp
  ops.log
}


case class MathOperations(ar: Array[Long], n: Int) {
  lazy val MOD = 998_244_353L

  def ars(resDefValue: Long = 1): (Array[Long], Array[Long]) = {
    val res = Array.ofDim[Long](n)
    res(0) = resDefValue
    val a = Array.ofDim[Long](1)
    a(0) = 1
    (res, a)
  }

  def log: Unit = {
    var (res, a) = ars(0)
    var f = MOD - 1
    for (i <- 1 until n) {
      a = multiplyTwoArrays(a, ar)
      f = -f + MOD
      for (j <- 0 until n) {
        res(j) = (res(j) + ((getValOrZero(a, j) * ((f * inv(i)) % MOD)) % MOD)) % MOD
      }
    }
    printList(res)
  }

  def exp: Unit = {
    var (res, a) = ars()
    var f = 1L
    for (i <- 1 until n) {
      a = multiplyTwoArrays(a, ar)
      f = (f * i) % MOD
      for (j <- 0 until n) {
        res(j) = (res(j) + (((inv(f) % MOD) * getValOrZero(a, j)) % MOD)) % MOD
      }
    }
    printList(res)
  }

  def sqrt: Unit = {
    var (res, a) = ars()

    for (i <- 1 until n) {
      a = multiplyTwoArrays(a, ar)
      var f = find(i)
      if (f < 0) {
        f += MOD
      }
      for (j <- 0 until n) {
        res(j) = (res(j) + ((f * getValOrZero(a, j)) % MOD)) % MOD
      }
    }
    printList(res)
  }

  def multiplyTwoArrays(f: Array[Long], s: Array[Long]): Array[Long] = {
    val newSize = min(s.length + f.length + 3, n)
    val res = Array.ofDim[Long](newSize)
    for (i <- 0 until newSize) {
      var j: Int = 0
      while (j <= i) {
        res(i) = (res(i) + ((getValOrZero(f, j) * getValOrZero(s, i - j)) % MOD)) % MOD
        j += 1
      }
    }
    var degree: Int = newSize - 1
    while (degree > 0 && res(degree) == 0) degree -= 1
    res.splitAt(degree + 1)._1
  }

  def reverse(a: Long, module: Long): Long =
    module match {
      case 1 => a
      case value if value % 2 == 1 => (a * reverse(a, module - 1)) % MOD
      case _ =>
        val rv = reverse(a, module / 2)
        (rv * rv) % MOD
    }

  def inv(a: Long): Long = reverse(a, MOD - 2)

  def find(n: Int): Long = {
    var a = 1L
    var b = 1L
    for (i <- 0 until n) {
      a = (a * ((1 - 2 * i + MOD) % MOD)) % MOD
      b = (b * (((i + 1) * 2) % MOD)) % MOD
    }
    (a * inv(b)) % MOD
  }

  lazy val getValOrZero: (Array[Long], Int) => Long = (lst, idx) => if (idx >= lst.length) 0 else lst(idx)

  private def printList(a: Array[Long]) {
    a.foreach(el => print(el + " "))
    println()
  }
}
