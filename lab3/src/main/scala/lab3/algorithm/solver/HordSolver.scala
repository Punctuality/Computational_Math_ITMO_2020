package lab3.algorithm.solver

import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._

// TODO Replace F[Option[A]] with OptionT[F, A]
case class HordSolver[A: Fractional, F[_] : Monad](accuracy: A, iterationsLimit: A, iterationsForced: A) extends EquationSolver[A, A, F] {

  private val limit: Int = iterationsLimit.toInt
  private val forced: Int = iterationsForced.toInt
  require(limit > 1 && forced > 1 && limit > forced)

  private val zero: A = implicitly[Fractional[A]].fromInt(0)

  private def computeIntersection(f: A => A, a: A, b: A): A =
    a - (b - a) / (f(b) - f(a)) * f(a)

//  @tailrec is Achieved when using cats Monad instances (which are stack-safe) !!!
  private def solver(f: A => A, leftBound: A, rightBound: A, xPrev: Option[A], iteration: Int): F[Option[(Int, A, A)]] =
    computeIntersection(f, leftBound, rightBound)
      .pure[F]
      .map (x => (leftBound, x, rightBound) -> (f(leftBound), f(x), f(rightBound)))
      .map{
        case ((_, x, _), (_, _, _))     if iteration > limit       => Some(Left(x))
        case ((_, x, _), (_, _, _))     if iteration > forced &&
                   (x - xPrev.getOrElse(leftBound)).abs < accuracy => Some(Left(x))
        case ((_, x, _), (_, y_x, _))   if y_x.abs < accuracy      => Some(Left(x))
        case ((a, x, _), (y_a, y_x, _)) if y_a * y_x < zero        => Some(Right(a, x, x))
        case ((_, x, b), (_, y_x, y_b)) if y_x * y_b < zero        => Some(Right(x, b, x))
        case _                                                     => None
      }.flatMap{
        case None                             => Option.empty[(Int, A, A)].pure[F]
        case Some(Left(result))               => (iteration + 1, result, if (iteration < 1) result else xPrev.getOrElse(leftBound)).some.pure[F]
        case Some(Right((left, right, xNow))) => solver(f, left, right, Some(xNow), iteration + 1)
      }

  override def findSolution(f: A => A, leftBound: A, rightBound: A): F[Option[(Int, A, A)]] =
    solver(f, leftBound, rightBound, None, 0)

}
