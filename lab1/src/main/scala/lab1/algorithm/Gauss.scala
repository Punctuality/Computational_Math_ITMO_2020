package lab1.algorithm

import lab1.math.Matrix

import lab1.math.Implicits._

object Gauss {
  def solveSystem(reprMatrix: Matrix[Double]): Either[String, List[(Int, Double)]] = if (reprMatrix.rows == reprMatrix.cols - 1) {
    val triangle = reprMatrix.triangulate
    println(s"Triangulated matrix:\n$triangle")
    if (triangle.getDiagonal.product == 0) {
      if (triangle.data.exists(row => row.forall(_ == 0))){
        Left("System has infinite amount of  solutions")
      } else {
        Left("System has no solutions")
      }
    } else {
      Right apply triangle.data.map{ row =>
        val shortenRow = row.dropWhile(_ == 0).toList
        shortenRow.map(_ / shortenRow.head)
      }.foldRight(List.empty[Double]){
        case (row, answers) => (row.last - (row.init.tail zip answers).map{ case (a, b) => a * b }.sum) +: answers
      }.zipWithIndex.map(_.swap)
    }
  } else {
    Left(s"${reprMatrix.signature} has invalid sizes for Gauss solving method")
  }
}
