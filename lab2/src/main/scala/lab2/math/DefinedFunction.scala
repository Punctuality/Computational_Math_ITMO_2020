package lab2.math

import scala.math._
import Ordering.Implicits._
import Fractional.Implicits._

class DefinedFunction[T : Fractional](
  val function: T => T,
  val derivatives: Map[Int, DefinedFunction[T]],
  val definedOnRange: List[DefinedRange[T]],
  val excludedPoints: List[BreakPoint[T]]
) extends (T => T) {

  override def apply(value: T): T = function(value)

  def get(value: T): Option[T] =
    if (definedOnRange.forall(_ contains value) && excludedPoints.forall(_.point != value)) Some(function(value)) else None

  def maxValue(range: DefinedRange[T]): T = {
    val approxN = (definedOnRange.map(_.length * implicitly[Numeric[T]].fromInt(10000)) :+ implicitly[Numeric[T]].fromInt(1_000_000)).min
    var maximum = implicitly[Numeric[T]] fromInt Int.MinValue

    var i = 0
    var curLeft = range.start
    val step = range.length / approxN

    while(i < approxN.toInt){
      val res = get(curLeft).getOrElse(maximum)
      maximum = if (maximum >= res) maximum else res
      curLeft += step
      i += 1
    }

    maximum
  }
}

object DefinedFunction{
  def apply[T: Fractional](
    function: T => T,
    derivatives: Map[Int, DefinedFunction[T]],
    definedOnRange: List[DefinedRange[T]],
    excludedPoints: List[BreakPoint[T]]
  ): DefinedFunction[T] = new DefinedFunction(function, derivatives, definedOnRange, excludedPoints)
}
