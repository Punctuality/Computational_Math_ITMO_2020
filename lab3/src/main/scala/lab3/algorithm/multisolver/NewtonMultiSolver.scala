package lab3.algorithm.multisolver

import cats.{Applicative, Monad}
import lab1.algorithm.Gauss
import lab1.math.MatrixDense
import lab1.math.Matrix._
import lab3.algorithm.solver.EquationSolver
import lab3.math.FromDouble
import lab3.math.FromDouble.Implicits._

import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._
import scala.reflect.ClassTag


class NewtonMultiSolver[A: Fractional: FromDouble, F[_] : Monad](accuracy: A, iterationsLimit: A)(implicit ctg: ClassTag[A])
  extends MultiEquationSolver[A, F] {

  private val limit: Int = iterationsLimit.toInt

  private def computeJacobian(func: List[A] => List[A], x: List[A])(implicit ctg: ClassTag[A]): Array[Array[A]] = {
    val EPS = 1E-5.fromDoubleTo[A]
    val d1 = func.apply(x)
    val J: Array[Array[A]] = new Array[Array[A]](d1.length).map(_ => new Array[A](x.length))
    for (i <- d1.indices) {
      val vals: Array[A] = x.toArray.clone
      vals(i) += EPS
      val d2: Array[A] = func.apply(vals.toList).toArray
      for (j <- d1.indices) {
        J(j)(i) = (d2(j) - d1(j)) / EPS
      }
    }
    J
  }

  override def findSolution(fs: List[A] => List[A], x: List[A]): F[Option[(Int, List[A])]] = {
    var xPrev: List[A] = x
    var xNow = x
    for (i <- 0 until limit){
      val jacob: Array[Array[A]] = computeJacobian(fs, xNow)
      val res: List[A] = fs(xNow)

      val matrixData = jacob.zip(res).map{
        case (jacAs: Array[A], fA: A) => (jacAs.toList :+ fA).toVector
      }.toVector

      val matrix: MatrixDense[Double] = MatrixDense.matrixFromFunction((i, j) => matrixData(i)(j).toDouble, matrixData.length, matrixData.head.length)

      Gauss.solveSystem(matrix) match {
        case Left(message) =>
          println(s"Newton solver: $message")
          return Applicative[F] pure Option.empty[(Int, List[A])]
        case Right(solution) =>
          xNow = implicitly[Numeric[Vector[A]]].minus(xNow.toVector, solution.map(_._2.fromDoubleTo[A]).toVector).toList

          val check = implicitly[Numeric[Vector[A]]].minus(xNow.toVector, xPrev.toVector).toList.zip(xPrev)

          if (check.forall{ case (now, prev) => (now - prev).abs <= accuracy}) {
            return Applicative[F] pure Some(i + 1 -> xNow)
          }
          xPrev = xNow
      }
    }
    Applicative[F] pure Some(limit -> xPrev)
  }

}
