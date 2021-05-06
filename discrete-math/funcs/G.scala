import java.util.Scanner

object G extends App {

  val in = new Scanner(new Scanner(System.in).nextLine.split("").mkString(" "))
  val rs = ans

  rs.foreach(i => print(i + " "))
  println

  def ans: Array[Long] = {
    val c = in.next.charAt(0)

    if (c == 'B') {
      Array(0L, 1L, 0L, 0L, 0L, 0L, 0L)
    } else {
      in.next
      val an: Array[Long] = ans
      in.next

      val res = Array.ofDim[Long](7)

      c match {
        case 'L' =>
          res(0) = 1
          for (
            i <- 1 until 7;
            j <- 1 to i
          ) res(i) += an(j) * res(i - j)
        case 'S' =>
          res(0) = 1
          for (
            i <- 1 until 7
          ) res(i) += mSet(i, i, an)
        case 'P' =>
          val r = ans
          in.next
          for (
            i <- 0 until 7;
            j <- 0 to i
          ) res(i) += an(j) * r(i - j)
      }
      res
    }
  }

  def mSet(n: Int, k: Int, res: Array[Long]): Long =
    if (n == 0) 1 else if (k == 0) 0 else (for (
      i <- 0 to n / k
    ) yield ch(res(k) + i - 1, i) * mSet(n - i * k, k - 1, res)).view.sum

  def ch(nT: Long, kT: Long): Long =
    (nT, kT) match {
      case (_, k) if k == 0 => 1
      case (n, k) if n < k => 0
      case (n, k) =>
        (n - k + 1 to n).foldLeft(1L)(_ * _) / (2L to k).foldLeft(1L)(_ * _)
    }
}
