import scala.io.StdIn.readLine

object F extends App {
  lazy val MOD = 1_000_000_007L

  val Array(k, m) = readLine().split(" ").map(_.toInt)

  val a = readLine().split(" ").map(_.toInt)
  val ans = Array.ofDim[Long](m + 1)
  val s = Array.ofDim[Long](m + 1)
  ans(0) = 1
  s(0) = 1
  for (i <- 1 to m) {
    var j = 0
    while (j < k) {
      if (i >= a(j)) ans(i) = (ans(i) + s(i - a(j))) % MOD
      j += 1
    }
    j = 0
    while (j <= i) {
      s(i) = (s(i) + ((ans(j) * ans(i - j)) % MOD)) % MOD
      j += 1
    }
  }
  ans.drop(1).foreach(el => print(el + " "))
}
