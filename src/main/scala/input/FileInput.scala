package input

import java.io.{File, FileInputStream}

import math.Matrix

import scala.reflect.ClassTag

class FileInput[A](file: File, parseFunc: String => A, sep: Char = ' ')(implicit tag: ClassTag[A]) extends MatrixInput[A] {
  private val reader = new StreamInput[A](parseFunc, sep, new FileInputStream(file))
  override def produceMatrix: Option[Matrix[A]] = reader.produceMatrix
}

object FileInput {
  def doubleFile (file: File, sep: Char = ' '): FileInput[Double]  = new FileInput(file, _.toDouble,  sep)
  def longFile   (file: File, sep: Char = ' '): FileInput[Long]    = new FileInput(file, _.toLong,    sep)
  def intFile    (file: File, sep: Char = ' '): FileInput[Int]     = new FileInput(file, _.toInt,     sep)
  def booleanFile(file: File, sep: Char = ' '): FileInput[Boolean] = new FileInput(file, _.toBoolean, sep)
}