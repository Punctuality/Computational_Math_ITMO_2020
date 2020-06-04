package lab5.math

import cats.{Id, Monad}
import core.input.FunctionParser
import core.math.FromDouble

class CustomDifferentialEquation[A: Fractional: FromDouble, F[_]: Monad] extends FunctionParser {
  def getODE(equation: String): OrdinaryDifferentialEquation[A, F] = {
    val func: (A, A) => F[A] = (x, y) => parseExpression(equation)(Map("x" -> x, "y" -> y))

    new OrdinaryDifferentialEquation[A, F] {
      override def value(xVal: A, yVal: A): F[A] = func(xVal, yVal)
      override def toString: String = s"y' = $equation"
    }
  }
}
