package lab1.math

import scala.Numeric.Implicits._

case class MatrixDense[A](override val data: Vector[Vector[A]]) extends Matrix[A] {
  val (rows, cols) = data.length -> data.headOption.map(row => row.length).getOrElse(0)

  if (data.exists(_.length != cols)) throw new RuntimeException("Data cannot be converted to Matrix")

  def dropRow(rowId: Int): Matrix[A] = MatrixDense apply data.patch(rowId, Vector.empty, 1)
  def dropCol(colId: Int): Matrix[A] = MatrixDense apply data.map(_.patch(colId, Vector.empty, 1))

  def getRow(rowId: Int): Vector[A] = data(rowId)
  def getCol(colId: Int): Vector[A] = data.map(_.apply(colId))

  def setRow(rowId: Int, row: Vector[A]): Matrix[A] = if (row.length == cols) {
    MatrixDense apply data.updated(rowId, row)
  } else {
    throw new RuntimeException("Row is not the same size as matrix is.")
  }
  def setCol(colId: Int, col: Vector[A]): Matrix[A] = if (col.length == rows) {
    MatrixDense apply data.zip(col).map{ case (row, newElem) => row.updated(colId, newElem)}
  } else {
    throw new RuntimeException("Row is not the same size as matrix is.")
  }

  def getValue(x: Int, y: Int): A  = data(x)(y)
  def setValue(x: Int, y: Int, value: A): Matrix[A] = MatrixDense apply data.updated(x, data(x).updated(y, value))

  def swapRow(aRowId: Int, bRowId: Int): Matrix[A] =
    MatrixDense apply data.updated(aRowId, data(bRowId)).updated(bRowId, data(aRowId))

  def equalsInSizeTo[B](that: Matrix[B]): Boolean = this.cols == that.cols && this.rows == that.rows
  def canBeMultipliedBy[B](that: Matrix[B]): Boolean = this.cols == that.rows

  def map[B](f: A => B): Matrix[B] = MatrixDense apply data.map(_.map(f))
  def mapRow(rowId: Int, f: A => A): Matrix[A] = MatrixDense apply data.updated(rowId, data(rowId).map(f))
  def mapCol(colId: Int, f: A => A): Matrix[A] = MatrixDense apply data.map(row => row.updated(colId, f(row(colId))))

  def multiplyBy(that: Matrix[A])(implicit numeric: Numeric[A]): Matrix[A] = if (this canBeMultipliedBy that) {
    MatrixDense.matrixFromFunction({
      case (x, y) => (this.getRow(x) zip that.getCol(y)).map { case (a, b) => a * b }.sum
    }, this.rows, that.cols)
  } else {
    throw new RuntimeException(s"Cannot multiply ${this.signature} by ${that.signature}")
  }

  def multiplyByNumber(num: A)(implicit numeric: Numeric[A]): Matrix[A] = this.map(_ * num)

  def zipWithMatrix[B](that: Matrix[B]): Matrix[(A, B)] = if (this equalsInSizeTo that) {
    MatrixDense apply (this.data zip that.data).map{ case (xL, yL) => xL zip yL }
  } else {
    throw new RuntimeException("Sizes are not equal")
  }

  def signature: String = s"Matrix($rows,$cols)"
  override def toString: String = data.map(row => s"[${row mkString " "}]") mkString "\n"
}

object MatrixDense{
  def matrixFromFunction[A](producer: (Int, Int) => A, rows: Int, cols: Int): MatrixDense[A] =
    MatrixDense apply (0 until rows).toVector.map(x => (0 until cols).toVector.map(y => producer(x, y)))
}