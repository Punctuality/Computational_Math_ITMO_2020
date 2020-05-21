package lab3.algorithm.separator

import cats._

import scala.annotation.tailrec
import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._

case class TabularSeparator[A: Fractional, B: Fractional, F[_]: Applicative](accuracy: A) extends SolutionSeparator[A, B, F] {

  type Dot = (A, B)
  type Result = List[(Dot, Dot)]

  private val zero: B = implicitly[Fractional[B]].fromInt(0)

  @tailrec
  private def separator(f: A => B, leftBound: A, rightBound: A, acc: Result = List.empty): Result =  {
    if (leftBound + accuracy >= rightBound) acc
    else {
      val (x1, x2): (A, A) = leftBound -> (leftBound + accuracy)
      val (y1, y2): (B, B) = f(x1)     -> f(x2)
      if (y1 * y2 < zero) separator(f, x2, rightBound, acc :+ ((x1, y1), (x2, y2)))
      else                separator(f, x2, rightBound, acc)
    }
  }

  override def separateSolutions(f: A => B, leftBound: A, rightBound: A): F[Result] =
    Applicative[F] pure separator(f, leftBound, rightBound)
}