package lab5.math

import cats.Monad
import cats.implicits._

import core.math.FromDouble
import core.math.FromDouble.Implicits._

import scala.math.Fractional.Implicits._
import scala.math._

trait OrdinaryDifferentialEquation[A, F[_]] {
  //y' = f(x, y)
  // double getValue(double xValue, double yValue);
  //    ConstEquation getGeneralSolution();
  //
  //    default Function getSpecificSolution(Point[] points) {
  //        return getGeneralSolution().getFunction(points);
  //    }

  def value(xVal: A, yVal: A): F[A]

  override def toString: String = ???
}

object OrdinaryDifferentialEquation {
  sealed trait Preset

  class Parabola[A: Fractional: FromDouble, F[_]: Monad] extends OrdinaryDifferentialEquation[A, F] with Preset {
    override def value(xVal: A, yVal: A): F[A] = pow(xVal.toDouble, 2).fromDoubleTo[A].pure[F]

    override def toString: String = "y' = x^2"
  }

  class Difference[A: Fractional: FromDouble, F[_]: Monad] extends OrdinaryDifferentialEquation[A, F] with Preset {
    override def value(xVal: A, yVal: A): F[A] = (xVal - yVal).pure[F]

    override def toString: String = "y' = x - y"
  }

  class SummingSinus[A: Fractional: FromDouble, F[_]: Monad] extends OrdinaryDifferentialEquation[A, F] with Preset {
    override def value(xVal: A, yVal: A): F[A] = (sin(xVal.toDouble).fromDoubleTo + yVal).pure[F]

    override def toString: String = "y' = sin(x) + y"
  }
}