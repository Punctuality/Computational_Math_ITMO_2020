package lab3.algorithm.solver

import cats._
import cats.implicits._

import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._

// TODO Replace F[Option[A]] with OptionT[F, A]
case class NewtonSolver[A: Fractional, F[_] : Monad](accuracy: A, iterationsLimit: A, iterationsForced: A) extends EquationSolver[A, A, F] {

  private val limit: Int = iterationsLimit.toInt
  private val forced: Int = iterationsForced.toInt
  require(limit > 1 && forced > 1 && limit > forced)

  private def computeDerivative(f: A => A, point: A, epsilon: A): A = (f(point + epsilon) - f(point)) / epsilon

  private def isIn(point: A, left: A, right: A): Boolean = (left <= point) && (point <= right)

  //  @tailrec is Achieved when using cats Monad instances (which are stack-safe) !!!
  private def solver(f: A => A, leftBound: A, rightBound: A, xPrev: Option[A], iteration: Int): F[Option[(Int, A, A)]] =
    xPrev.getOrElse(leftBound)
      .pure[F]
      .map(x => (x, f(x) / computeDerivative(f, x, accuracy), f(x)))
      .map{
        case (x, h, _) if iteration > limit                => Some(Left(x - h))
        case (x, h, _) if iteration > forced &&
          (x - xPrev.getOrElse(leftBound)).abs <= accuracy => Some(Left(x - h))
        case (x, h, _) if iteration > forced &&
                                         h.abs <= accuracy => Some(Left(x - h))
        case (x, h, f) if f.abs <= accuracy                => Some(Left(x - h))
        case (x, h, _) if isIn(x-h, leftBound, rightBound) => Some(Right(x - h))
        case _                                             => None
      }.flatMap{
        case None               => Option.empty[(Int, A, A)].pure[F]
        case Some(Left(result)) => (iteration + 1,  result, if (iteration < 10) result else xPrev.getOrElse(leftBound)).some.pure[F]
        case Some(Right(xNow))  => solver(f, leftBound, rightBound, Some(xNow), iteration + 1)
      }

  override def findSolution(f: A => A, leftBound: A, rightBound: A): F[Option[(Int, A, A)]] =
    solver(f, leftBound, rightBound, None, 0)

}
