import java.util.Scanner

object H extends App {
  lazy val getValOrZeroAr: (Array[Long], Int) => Long = (lst, idx) => if (idx >= lst.length) 0 else lst(idx)
  lazy val MOD = 998_244_353L

  val in = new Scanner(System.in)
  val k = in.nextInt
  val n = in.nextInt
  val c = Array.ofDim[Long](k + 1, k + 1)
  for (i <- 0 to k) {
    c(i)(0) = 1
    c(i)(i) = 1
    for (j <- 1 until i) {
      c(i)(j) = (c(i - 1)(j - 1) + c(i - 1)(j)) % MOD
      c(i)(j) = c(i)(j) + (if (c(i)(j) < 0) MOD else 0)
    }
  }
  val l = Array.ofDim[Long](k + 1)
  val r = Array.ofDim[Long](k + 1)
  for (i <- 0 until k - 1 if 2 * i < k - 1) l(i) = c(k - i - 2)(i) * (if (i % 2 == 0) 1 else -1)
  for (i <- 0 until k if 2 * i < k) r(i) = c(k - i - 1)(i) * (if (i % 2 == 0) 1 else -1)

  val t = Array.ofDim[Long](n + 1)
  t(0) = 1 / getValOrZeroAr(r, 0)
  for (i <- 0 to n) {
    for (j <- 0 until i) {
      t(i) = (t(i) - ((t(j) * getValOrZeroAr(r, i - j)) % MOD)) % MOD
      t(i) = t(i) + (if (t(i) < 0) MOD else 0)
    }
    t(i) /= getValOrZeroAr(r, 0)
  }
  for (i <- 0 until n) {
    var ans = 0L
    for (j <- 0 to i) {
      ans = (ans + ((getValOrZeroAr(l, j) * getValOrZeroAr(t, i - j)) % MOD)) % MOD
      ans = ans + (if (ans < 0) MOD else 0)
    }
    println(ans)
  }
}
