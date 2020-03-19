package input
import math.Matrix

import scala.reflect.ClassTag

class ConsoleInput[A](parseFunc: String => A, sep: Char = ' ')(implicit tag: ClassTag[A]) extends MatrixInput[A] {
  private val reader = new StreamInput[A](parseFunc, sep, System.in)
  override def produceMatrix: Option[Matrix[A]] = {
    println(s"Input dimensions (rows cols), then (row by row) input matrix values (sep = '$sep'):")
    reader.produceMatrix
  }
}

object ConsoleInput {
  def doubleConsole (sep: Char = ' '): ConsoleInput[Double]  = new ConsoleInput(_.toDouble,  sep)
  def longConsole   (sep: Char = ' '): ConsoleInput[Long]    = new ConsoleInput(_.toLong,    sep)
  def intConsole    (sep: Char = ' '): ConsoleInput[Int]     = new ConsoleInput(_.toInt,     sep)
  def booleanConsole(sep: Char = ' '): ConsoleInput[Boolean] = new ConsoleInput(_.toBoolean, sep)
}
