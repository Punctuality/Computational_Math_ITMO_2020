package lab2.math

sealed trait BreakPoint[T] {
  def point: T
}

case class  FirstOrderPoint[T: Fractional](point: T) extends BreakPoint[T]
case class SecondOrderPoint[T: Fractional](point: T) extends BreakPoint[T]