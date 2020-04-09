package lab2.math

import Ordering.Implicits._
import Numeric.Implicits._
import scala.math._

case class DefinedRange[T: Fractional](start: T, stop: T){
  def contains(value: T): Boolean = (start <= value) && (value <= stop)

  def length: T = {
    val length = start - stop
    if (length > implicitly[Numeric[T]].fromInt(0)) length else -length
  }
}

object DefinedRange{
  def get[T: Fractional](start: T, stop: T): DefinedRange[T] = if (start <= stop) {
    DefinedRange(start, stop)
  } else {
    DefinedRange(stop, start)
  }
}
