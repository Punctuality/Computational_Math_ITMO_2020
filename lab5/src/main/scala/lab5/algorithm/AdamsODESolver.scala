package lab5.algorithm

import cats.{Id, Monad}
import cats.implicits._
import core.math.FromDouble
import lab5.math.OrdinaryDifferentialEquation

import scala.language.implicitConversions
import scala.math.Fractional.Implicits._
import scala.math.Ordering.Implicits._
import scala.reflect.ClassTag

class AdamsODESolver[A: Fractional: FromDouble: ClassTag, F[_]: Monad](var accuracy: A) extends ODESolver[A, F, Id] {

  private implicit def intToFrac[N: Numeric](int: Int): N = implicitly[Numeric[N]].fromInt(int)

  private val rungeKuttaODESolver = new RungeKuttaODESolver[A, F]

  override def solveODE(ode: OrdinaryDifferentialEquation[A, Id], startPoint: (A, A), step: A, count: Int): F[List[(A, A)]] =
      rungeKuttaODESolver.solveODE(ode, startPoint, step, 4).map { initPoints: List[(A, A)] =>
          val yDerivative = new Array[A](count)

          Range(0, 4).foreach { i =>
            yDerivative(i) = initPoints(i) match {
              case (x, y) => ode.value(x, y)
            }
          }

          var (x, y) = initPoints.last
          val multiStepPoints =
            Range(4, count).map { i =>
              // Prediction
              var newY: A = step * (
                yDerivative(i - 1) * 55 -
                  yDerivative(i - 2) * 59 +
                  yDerivative(i - 3) * 37 -
                  yDerivative(i - 4) * 9
                ) / 24 + y

              x += step
              // Correction
              var prevY: A = newY

              do {
                prevY = newY
                yDerivative(i) = ode.value(x, prevY)
                newY = step * (
                  yDerivative(i - 0) * 9 +
                    yDerivative(i - 1) * 19 -
                    yDerivative(i - 2) * 5 +
                    yDerivative(i - 3)
                  ) / 24 + y
              } while ((prevY - newY).abs > accuracy)

              y = newY

              x -> y
            }.toList

          initPoints ++ multiStepPoints
      }
}
