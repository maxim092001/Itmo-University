import java.util.Scanner

object I extends App {
//   lazy val MOD = 104_857_601L s constantoi ne zashlo, pscms durak

  val in: Scanner = new Scanner(System.in)

  val k = in.nextInt
  val n = in.nextLong - 1
  val a = Array.ofDim[Long](2 * k)
  val c = Array.ofDim[Long](k + 1)
  //  val c = Array(0) ++ (for (_ <- 1 to k) yield (-in.nextInt + MOD) % MOD).toArray

  for (i <- 0 until k) a(i) = in.nextInt
  c(0) = 1
  for (i <- 1 to k) c(i) = (-in.nextInt + 104_857_601L) % 104_857_601L

  println(Ops.res(n, k, a, c))
}

object Ops {

  lazy val getValOrZero: (Array[Long], Int) => Long = (lst, idx) => if (idx >= lst.length) 0 else lst(idx)

  def res(n: Long, k: Int, a: Array[Long], c: Array[Long]): Long = {
    var n1 = n
    while (n1 >= k) {
      for (i <- k until 2 * k) {
        a(i) = 0
        for (j <- 1 to k) {
          a(i) = (a(i) - ((c(j) * a(i - j)) % 104_857_601L)) % 104_857_601L
          a(i) = a(i) + (if (a(i) < 0) 104_857_601L else 0)
        }
      }
      val s = Array.ofDim[Long](k + 1)
      for (i <- 0 to k by 2) s(i) = c(i)
      for (i <- 1 to k by 2) s(i) = (-c(i) + 104_857_601L) % 104_857_601L
      val m = multiplyTwoArrays(c, s)
      for (i <- 0 to k) c(i) = m(i * 2)
      for (i <- (n1 % 2).toInt until 2 * k by 2) a(i / 2) = a(i)
      n1 /= 2
    }
    a(n1.toInt)
  }

  private def multiplyTwoArrays(f: Array[Long], s: Array[Long]): Array[Long] = {
    val newSize = s.length + f.length + 3
    val res = Array.ofDim[Long](newSize)
    for (i <- 0 until newSize by 2) {
      var j: Int = 0
      while (j <= i) {
        res(i) = (res(i) + ((getValOrZero(f, j) * getValOrZero(s, i - j)) % 104_857_601L)) % 104_857_601L
        j += 1
      }
    }
    res
  }
}
