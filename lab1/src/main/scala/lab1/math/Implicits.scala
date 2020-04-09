package lab1.math

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.math.{Fractional, Numeric, min, pow}
import scala.reflect.ClassTag

import lab1.math.Matrix._

object Implicits{
  import Fractional.Implicits._
  import Numeric.Implicits._

  implicit class RichMatrix[A : ClassTag](matrix: Matrix[A]) {
    import ExecutionContext.Implicits.global

    def transpose: Matrix[A] = MatrixDense apply matrix.data.transpose
    def T: Matrix[A] = transpose

    def getDiagonal: Vector[A] = (0 until min(matrix.rows, matrix.cols)).toVector.map(index => matrix.getValue(index, index))

    def triangulate(implicit fractional: Fractional[A]): Matrix[A] =
      (0 until (matrix.rows - 1)).foldLeft(matrix){
        case (unorderedMatrix, rowId) =>
          val (notAdjustedMatrix, sigh) = RichMatrix.orderingRowsByZeros(unorderedMatrix, rowId)
          val matrix = notAdjustedMatrix.mapRow(0, implicitly[Numeric[A]].times(_, implicitly[Numeric[A]].fromInt(sigh)))

          val a = matrix.getRow(rowId).apply(rowId)
          ((rowId + 1) until matrix.rows).foldLeft(matrix){
            case (matrix, toChangeId) =>
              val b = matrix.getRow(toChangeId).apply(rowId)
              if (a != implicitly[Fractional[A]].fromInt(0)) {
              matrix.setRow(toChangeId, matrix.getRow(toChangeId) - matrix.getRow(rowId).map(implicitly[Numeric[A]].times(_, b / a)))
              } else matrix
          }
      }

    private def parallel_determinant_by_minors(implicit numeric: Numeric[A]): Future[A] = if (matrix.rows == matrix.cols) {
      if (matrix.rows > 1){
        Future.traverse(0 until matrix.cols toList){ colId =>
          val sign  = implicitly[Numeric[A]] fromInt pow(-1, colId).toInt
          val coef  = matrix.getValue(0, colId)
          val minor = matrix.dropRow(0).dropCol(colId)
          Future apply minor.determinant_by_minors * sign * coef
        }.map(_.sum)
      } else {
        Future successful matrix.getValue(0, 0)
      }
    } else {
      throw new RuntimeException(s"$matrix is not square.")
    }

    def determinant_by_minors(implicit numeric: Numeric[A]): A = Await.result(parallel_determinant_by_minors, Duration.Inf)

    def determinant_by_triangulation(implicit fractional: Fractional[A]): (Matrix[A], A) = if (matrix.rows == matrix.cols) {
      val triangle = matrix.triangulate
      triangle -> triangle.getDiagonal.product
    } else {
      throw new RuntimeException(s"$matrix is not square.")
    }

    def determinant(implicit fractional: Fractional[A]): A = determinant_by_triangulation._2
  }

  object RichMatrix {
    def orderingRowsByZeros[N: Numeric](matrix: Matrix[N], offset: Int = 0): (Matrix[N], Int) = {
      val enriched = matrix.data.drop(offset)
        .map( row => (row.takeWhile(_ == implicitly[Numeric[N]].fromInt(0)).length, row) )
      // This is done for the purpose of knowing how many times we would have swapped row.
      var minSwaps = 0
      for {
        i <- 0 until (enriched.length - 1)
        j <- i + 1 until enriched.length
      } if (enriched(i)._1 > enriched(j)._1) minSwaps += 1
      // Because with using "sortBy" we cannot count that number.
      // But it's crucial while calculating determinant
      val sorted = enriched.sortBy(_._1)
      MatrixDense(matrix.data.take(offset) ++ sorted.map(_._2)) -> ((minSwaps % 2) * 2 - 1)
    }
  }
}
