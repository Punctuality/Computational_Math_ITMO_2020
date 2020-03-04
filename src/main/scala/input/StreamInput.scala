package input

import java.io.{FileInputStream, InputStream}
import java.util.Scanner

import math.{Matrix, MatrixDense}

import scala.util.Try

class StreamInput[A](parseFunc: String => A, sep: Char, inputStream: InputStream) extends MatrixInput[A]{
  val lineScanner = new Scanner(inputStream)
  override def produceMatrix: Option[Matrix[A]] = {
    Try(lineScanner.nextLine.split(' ').toList.map(_.toInt)).collect {
      case m :: n :: _ if m > 0 && n > 0 =>
        MatrixDense apply (0 until m).toVector.map{_ =>
          lineScanner.nextLine.split(sep).take(n).map(parseFunc).toVector
        }
    }.toOption
  }
}
