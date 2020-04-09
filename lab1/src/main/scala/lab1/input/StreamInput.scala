package lab1.input

import java.io.InputStream
import java.util.Scanner

import lab1.math.{MatrixDense, Matrix}

import scala.reflect.ClassTag
import scala.util.Try

class StreamInput[A](parseFunc: String => A, sep: Char, inputStream: InputStream)(implicit tag: ClassTag[A]) extends MatrixInput[A]{
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
