package algorithm

import math.Matrix
import math.Implicits._

object Gauss {
  def solveSystem(reprMatrix: Matrix[Double]): Either[String, List[(Int, Double)]] = if (reprMatrix.rows == reprMatrix.cols - 1) {
    val triangle = reprMatrix.triangulate
    if (triangle.getDiagonal.product == 0) {
      Left("System has no solutions")
    } else {
      Right apply triangle.data.map{ row =>
        val shortenRow = row.dropWhile(_ == 0).toList
        shortenRow.map(_ / shortenRow.head)
      }.reverse.map{a =>
        println(a)
        a
      }.foldLeft(List.empty[Double]){
        case (answers, row) => (row.last - (row.init.tail zip answers).map{ case (a, b) => a * b }.sum) +: answers
      }.zipWithIndex.map(_.swap)
    }
  } else {
    Left(s"${reprMatrix.signature} has invalid sizes for Gauss solving method")
  }
}
