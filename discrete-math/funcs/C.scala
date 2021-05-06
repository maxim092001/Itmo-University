import java.util.Scanner

object C extends App {
  lazy val MOD = 1_000_000_000_000L

  val in: Scanner = new Scanner(System.in)
  val k: Int = in.nextInt()
  val a = Array.ofDim[Long](k)
  val c = Array.ofDim[Long](k)
  val q = Array.ofDim[Long](k + 1)
  for (i <- 0 until k) {
    a(i) = in.nextLong()
  }
  var i: Int = 0
  while (i < k) {
    c(i) = in.nextLong()
    i += 1
  }
  q(0) = 1
  i = 0
  while (i < k) {
    q(i + 1) = -c(i)
    i += 1
  }

  val getValOrZero: (Array[Long], Int) => Long = (lst, idx) => if (idx >= lst.length) 0 else lst(idx)

  val result: Array[Long] = Array.ofDim[Long](a.length +

    q.length + 1)
  for (i <- result.indices) {
    var j: Int = 0
    while (j <= i) {
      result(i) = (result(i) + ((getValOrZero(a, j) * getValOrZero(q, i - j)) % MOD)) % MOD
      j += 1
    }
  }
  var degree: Int = if (result.length >= k + 1) k - 1 else result.length - 1
  while (degree > 0 && result(degree) == 0) degree -= 1

  printList(result.splitAt(degree + 1)._1)
  printList(q)

  private def printList(a: Array[Long]) {
    println(a.length - 1)
    a.foreach(el => print(el + " "))
    println()
  }
}