package lab2.algorithm

import lab2.math.{BreakPoint, DefinedRange}

sealed trait IntegrateError

case class SecondOrderBreakPointError[T](point: BreakPoint[T]) extends IntegrateError
case class OutOfRangeBounds[T](definedRange: DefinedRange[T]) extends IntegrateError
case class NoProperDerivative(order: Int) extends IntegrateError
