import java.util.Scanner
import scala.math._

object E extends App {

  import MathOperationsE._

  val in = new Scanner(System.in)
  val r = in.nextInt
  val d = in.nextInt

  val p = Array.ofDim[Int](d + 1)

  for (i <- 0 to d) p(i) = in.nextInt

  val pw = Array.ofDim[Long](12)

  pw(0) = 1
  for (i <- 1 to 11) pw(i) = pw(i - 1) * r

  val t = Array(1L, -r)
  var f = Array(0L)
  var z = Array(1L)

  for (
    i <- d to 0 by -1
  ) {
    val q: Array[Long] = (if (i == 0) {
      1L :: Nil
    } else {
      0L :: (for (
        j <- 0 until i
      ) yield euler(i)(j) * pw(j)
      ).toList
    }).toArray
    f = sum(f, multiplyTwoArrays(q, z), p(i) * (if (i == 0) 1 else r))
    z = multiplyTwoArrays(z, t)
  }

  println(f.length - 1)
  f.foreach(el => print(el + " "))
  println
  println(z.length - 1)
  z.foreach(el => print(el + " "))


  lazy val euler = Array(
    Array(1),
    Array(1),
    Array(1, 1),
    Array(1, 4, 1),
    Array(1, 11, 11, 1),
    Array(1, 26, 66, 26, 1),
    Array(1, 57, 302, 302, 57, 1),
    Array(1, 120, 1191, 2416, 1191, 120, 1),
    Array(1, 247, 4293, 15619, 15619, 4293, 247, 1),
    Array(1, 502, 14608, 88234, 156190, 88234, 14608, 502, 1),
    Array(1, 1013, 47840, 455192, 1310354, 1310354, 455192, 47840, 1013, 1),
  )

}

object MathOperationsE {

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

  def sum(f: Array[Long], s: Array[Long], coefficient: Long = 1L): Array[Long] = {
    val newSize = max(f.length, s.length) + 1
    val ans: Array[Long] = Array.ofDim[Long](newSize)
    for (
      i <- f.indices
    ) ans(i) += f(i)
    for (
      i <- s.indices
    ) ans(i) += s(i) * coefficient
    var degree: Int = newSize - 1
    while (degree > 0 && ans(degree) == 0) degree -= 1
    ans.splitAt(degree + 1)._1
  }
}
