package lab5.algorithm

import cats.{Id, Monad}
import cats.implicits._
import core.math.FromDouble
import lab5.math.OrdinaryDifferentialEquation

import scala.language.implicitConversions
import scala.math.Fractional.Implicits._

class RungeKuttaODESolver[A: Fractional: FromDouble, F[_]: Monad] extends ODESolver[A, F, Id] {

  private implicit def intToFrac[N: Numeric](int: Int): N = implicitly[Numeric[N]].fromInt(int)

  override def solveODE(ode: OrdinaryDifferentialEquation[A, Id], startPoint: (A, A), step: A, count: Int): F[List[(A, A)]] =
    Monad[F] pure LazyList.iterate[(A, A)](startPoint){
        case (x, y) =>
          val k0 = step * ode.value(x, y)
          val k1 = step * ode.value(x + step / 2, y + k0 / 2)
          val k2 = step * ode.value(x + step / 2, y + k1 / 2)
          val k3 = step * ode.value(x + step, y + k2)

          (x + step) -> (y + (k0 + k1 * 2 + k2 * 2 + k3) / 6)
      }.take(count).toList
}
