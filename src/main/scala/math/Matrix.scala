package math

import java.io.ByteArrayInputStream

import input.StreamInput

import Numeric.Implicits._
import scala.reflect.ClassTag
import scala.util.Try

trait Matrix[A]{
  def rows: Int
  def cols: Int
  def data: Vector[Vector[A]]

  def dropRow(rowId: Int): Matrix[A]
  def dropCol(colId: Int): Matrix[A]

  def getRow(rowId: Int): Vector[A]
  def getCol(colId: Int): Vector[A]

  def setRow(rowId: Int, row: Vector[A]): Matrix[A]
  def setCol(colId: Int, col: Vector[A]): Matrix[A]

  def getValue(x: Int, y: Int): A
  def setValue(x: Int, y: Int, value: A): Matrix[A]

  def swapRow(aRowId: Int, bRowId: Int): Matrix[A]

  def equalsInSizeTo[B](that: Matrix[B]): Boolean
  def canBeMultipliedBy[B](that: Matrix[B]): Boolean

  def map[B](f: A => B): Matrix[B]
  def mapRow(rowId: Int, f: A => A): Matrix[A]
  def mapCol(colId: Int, f: A => A): Matrix[A]

  def multiplyBy(that: Matrix[A])(implicit numeric: Numeric[A]): Matrix[A]
  def x(that: Matrix[A])(implicit numeric: Numeric[A]): Matrix[A] = multiplyBy(that)

  def multiplyByNumber(num: A)(implicit numeric: Numeric[A]): Matrix[A]
  def *#(num: A)(implicit numeric: Numeric[A]): Matrix[A] = multiplyByNumber(num)

  def zipWithMatrix[B](that: Matrix[B]): Matrix[(A, B)]
  
  def signature: String
}

object Matrix{
  def equalInSize[A, B](x: Matrix[A], y: Matrix[B]): Boolean = x equalsInSizeTo y
  def canMultiply[A, B](x: Matrix[A], y: Matrix[B]): Boolean = x canBeMultipliedBy y
  def multiply[N : Numeric](x: Matrix[N], y: Matrix[N]): Matrix[N] = x multiplyBy y
  def zipMatrices[A, B](x: Matrix[A], y: Matrix[B]): Matrix[(A, B)] = x zipWithMatrix y

  implicit def matrixNumeric[N : Numeric : ClassTag]: Numeric[Matrix[N]] = new Numeric[Matrix[N]] {
    override def plus(x: Matrix[N], y: Matrix[N]): Matrix[N] = zipMatrices(x, y).map { case (xs, ys) => xs + ys }

    override def minus(x: Matrix[N], y: Matrix[N]): Matrix[N] = x + (-y)

    override def times(x: Matrix[N], y: Matrix[N]): Matrix[N] = zipMatrices(x, y).map { case (xs, ys) => xs * ys }

    override def negate(x: Matrix[N]): Matrix[N] =
      x.map(-_)

    // Basically those methods are not for matrix

    override def fromInt(x: Int): Matrix[N] = ???

    override def toInt(x: Matrix[N]): Int = ???

    override def toLong(x: Matrix[N]): Long = ???

    override def toFloat(x: Matrix[N]): Float = ???

    override def toDouble(x: Matrix[N]): Double = ???

    override def compare(x: Matrix[N], y: Matrix[N]): Int = ???

    override def parseString(str: String): Option[Matrix[N]] = {
      val inputStream = new ByteArrayInputStream(str.getBytes)
      val input = new StreamInput[N](implicitly[Numeric[N]].parseString(_).get, ' ', inputStream)
      input.produceMatrix
    }
  }

  implicit def vectorNumeric[N : Numeric : ClassTag]: Numeric[Vector[N]] = new Numeric[Vector[N]] {
    override def plus(x: Vector[N], y: Vector[N]): Vector[N] = x.zip(y).map{ case (xs, ys) => xs + ys }

    override def minus(x: Vector[N], y: Vector[N]): Vector[N] = x + (-y)

    override def times(x: Vector[N], y: Vector[N]): Vector[N] = x.zip(y).map{ case (xs, ys) => xs * ys }

    override def negate(x: Vector[N]): Vector[N] = x.map(-_)

    // Basically those methods are not for vector

    override def fromInt(x: Int): Vector[N] = ???

    override def toInt(x: Vector[N]): Int = ???

    override def toLong(x: Vector[N]): Long = ???

    override def toFloat(x: Vector[N]): Float = ???

    override def toDouble(x: Vector[N]): Double = ???

    override def compare(x: Vector[N], y: Vector[N]): Int = ???

    override def parseString(str: String): Option[Vector[N]] = Try(str.split(' ').flatMap(implicitly[Numeric[N]].parseString).toVector).toOption
  }
}
