import scala.io.StdIn._
import scala.math._
import utils._
import scala.collection.mutable.ArrayBuffer

object Main extends App {

  val Array(n, m, k) = readI
  val nodes          = Node.readN(n, m)
  nodes
    .takeRight(k)
    .foreach(x => {
      val matrix = x.calcF
      matrix.printSelf()
      println()
      x.addDiff(Matrix.readM(matrix.rows, matrix.cols))
    })

  nodes.reverse.foreach(_.pDiff())
  nodes
    .take(m)
    .foreach(n => {
      n.diffM.printSelf()
      println()
    })
}

trait Node {

  private var funC: Option[Matrix] = None
  var diff: Option[Matrix]         = None

  def calcFIn: Matrix
  def calcF: Matrix = {
    if (funC.isEmpty)
      funC = Some(calcFIn)
    funC.get
  }

  private lazy val zero: Matrix = {
    val m = calcF
    Matrix.matrixE(m.rows, m.cols, 0)
  }

  lazy val diffM: Matrix = diff.getOrElse(zero)

  def addDiff(d: Matrix): Unit = {
    if (diff.isEmpty)
      diff = Some(d)
    else
      diff = diff.map(x => x + d)
  }

  def pDiff(): Unit
}

object Node {

  def readN(n: Int, m: Int): IndexedSeq[Node] = {

    val res = ArrayBuffer[Node]()
    for {
      _ <- 0 until n
    } yield {

      val r      = readL.toIndexedSeq
      val name   = r.head
      val values = r.tail.map(_.toInt)

      res += (name match {

        case "var" => VarNode(values(0), values(1))
        case "tnh" => ThnNode(res(values(0) - 1))
        case "rlu" => RluNode(1.0 / values(0), res(values(1) - 1))
        case "mul" => MulNode(res(values(0) - 1), res(values(1) - 1))
        case "sum" => SumNode(values(0), values.tail.map(x => res(x - 1)))
        case "had" => HadNode(values(0), values.tail.map(x => res(x - 1)))
      })
    }

    for (i <- 0 until m) yield {
      val inputNode = res(i).asInstanceOf[VarNode]
      inputNode.m = Some(Matrix.readM(inputNode.rows, inputNode.cols))
    }
    res.toIndexedSeq
  }

  case class VarNode(rows: Int, cols: Int) extends Node {

    var m: Option[Matrix] = None

    override def calcFIn: Matrix = m.get

    override def pDiff(): Unit = ()

  }

  case class ThnNode(n: Node) extends Node {

    override def calcFIn: Matrix = n.calcF.map { tanh }

    override def pDiff(): Unit =
      n.addDiff(calcF.mapWithIndex((row, col, v) => (1.0 - v * v) * diff.get.matrix(row)(col)))

  }

  case class RluNode(alpha: Double, n: Node) extends Node {

    override def calcFIn: Matrix = n.calcF.map { x => max(x, x * alpha) }

    override def pDiff(): Unit =
      n.addDiff(n.calcF.mapWithIndex((row, col, v) => diff.get.matrix(row)(col) * (if (v < 0.0) alpha else 1.0)))

  }

  case class MulNode(left: Node, right: Node) extends Node {

    override def calcFIn: Matrix = left.calcF ** right.calcF

    override def pDiff(): Unit = {
      left.addDiff(diff.get.times(right.calcF, transpose = true))
      right.addDiff(left.calcF.times(diff.get, transposeCur = true))
    }

  }

  case class SumNode(l: Int, args: IndexedSeq[Node]) extends Node {

    override def calcFIn: Matrix =  args.tail.foldLeft[Matrix](args.head.calcF)((acc, cur) => acc + cur.calcF)

    override def pDiff(): Unit = args.foreach(_.addDiff(diff.get))

  }

  case class HadNode(l: Int, args: IndexedSeq[Node]) extends Node {

    override def calcFIn: Matrix = args.tail.foldLeft[Matrix](args.head.calcF)((acc, cur) => acc * cur.calcF)

    override def pDiff(): Unit = args.zipWithIndex.foreach {
      case (n, idxA) =>

        val e = args.head.calcF

        n.addDiff(
          args.zipWithIndex.foldLeft(Matrix.matrixE(e.rows, e.cols, 1.0))((m, p) =>
            if (p._2 == idxA) m else m * p._1.calcF
          ) * diff.get
        )
    }

  }
}

case class Matrix(rows: Int, cols: Int, matrix: IndexedSeq[IndexedSeq[Double]]) {

  def map(f: Double => Double): Matrix = this.copy(matrix = matrix.map(_.map(f)))

  def mapWithIndex(f: (Int, Int, Double) => Double): Matrix =
    this.copy(matrix = matrix.zipWithIndex.map { case (lst, idxR) =>
      lst.zipWithIndex.map { case (el, idxC) => f(idxR, idxC, el) }
    })

  def +(m: Matrix): Matrix = mapWithIndex((row, col, el) => el + m.matrix(row)(col))

  def *(m: Matrix): Matrix = mapWithIndex((row, col, el) => el * m.matrix(row)(col))

  def **(m: Matrix): Matrix = times(m)

  def times(m: Matrix, transposeCur: Boolean = false, transpose: Boolean = false): Matrix = {

    val x1 = if (transposeCur) rows else cols
    val x2 = if (transpose) m.cols else m.rows

    assert(x1 == x2)

    val n = if (transposeCur) cols else rows
    val k = if (transpose) m.rows else m.cols

    val resMatrix: Array[Array[Double]] = (for {
      _ <- 0 until n
    } yield {
      (0 until k).map(_ => 0.0).toArray
    }).toArray

    for {
      i <- 0 until n
      j <- 0 until k
      l <- 0 until x1
    } yield {

      val left  = if (transposeCur) matrix(l)(i) else matrix(i)(l)
      val right = if (transpose) m.matrix(j)(l) else m.matrix(l)(j)
      resMatrix(i).update(j, resMatrix(i)(j) + left * right)
    }

    Matrix(n, k, resMatrix.map(_.toIndexedSeq).toIndexedSeq)
  }

  def printSelf(): Unit =
    for {
      i <- 0 until rows
    } yield {
      for {
        j <- 0 until cols
      } yield {
        print(s"${matrix(i)(j)} ")
      }
      println()
    }
}

object Matrix {

  def readM(rows: Int, cols: Int): Matrix =
    Matrix(
      rows,
      cols,
      (0 until rows).map(_ => readD.toIndexedSeq)
    )

  def matrixE(rows: Int, cols: Int, v: Double): Matrix =
    Matrix(
      rows,
      cols,
      (0 until rows).map(_ => (0 until cols).map(_ => v))
    )
}

object utils {

  def readL: Array[String] = readLine.split("\\s+")

  def readD: Array[Double] = readL.map(_.toDouble)

  def readI: Array[Int] = readL.map(_.toInt)
}
