package lab2.algorithm

import lab2.math._
import scala.math._

class SimpsonIntegral(function: DefinedFunction[Double]) {

  private val eps: Double = Double.MinPositiveValue * 2.0

  def result(range: DefinedRange[Double], accuracy: Double): Either[IntegrateError, (Double, Double, Int)] =
    Either.cond(
      function.definedOnRange.exists{case DefinedRange(start, stop) => start <= range.start && range.stop <= stop},
      integrate_cycle(range, countSections(range, accuracy), accuracy) ,
      OutOfRangeBounds(range)
    ).joinRight

  private def integrate_cycle(range: DefinedRange[Double], nSections: Int, accuracy: Double): Either[IntegrateError, (Double, Double, Int)] = for {
      normalPrecision <- integrate(range, nSections)
      doubledPrecision <- integrate(range, 2 * nSections)
      result <- if (abs(doubledPrecision - normalPrecision) > accuracy) {
        integrate_cycle(range, 2 * nSections, accuracy)
      } else {
        Right(doubledPrecision, doubledPrecision - normalPrecision, nSections)
      }
    } yield result

  private def integrate(range: DefinedRange[Double], nSections: Int): Either[IntegrateError, Double] =
    function.excludedPoints.collectFirst{
      case bp@SecondOrderPoint(point) if range.contains(point) => SecondOrderBreakPointError(bp)
    }.toLeft{
      val firstOrderPoints = function.excludedPoints.collect{
        case FirstOrderPoint(point) if range.contains(point) => point
      }.sorted :+ (range.stop + eps)

      firstOrderPoints.foldLeft((range, List.empty[DefinedRange[Double]])) {
        case ((DefinedRange(start, stop), result), point) => DefinedRange(point + eps, stop) -> (DefinedRange(start, point - eps) +: result)
      }._2 match {
        case DefinedRange(start, stop) :: Nil =>
          val step = (stop - start) / (nSections - nSections % 2 + 1)
          val xs = LazyList.iterate(start + eps)(_ + step).take(nSections - nSections % 2 + 1).iterator
          Right apply xs.sliding(3, 2).map(_.map(function).toList).map{
            case yPrev :: yMid :: yNext :: Nil => yPrev + (4 * yMid) + yNext
          }.sum * step / 3.0
        case ranges => ranges.map(integrate(_, nSections)).reduce((a, b) => a.flatMap(aVal => b.map(aVal + _)))
      }
    }.joinRight

  private def countSections(range: DefinedRange[Double], accuracy: Double): Int =
    function.derivatives.get(4).toRight(
      NoProperDerivative(4)
    ).flatMap( derivative =>
      derivative.excludedPoints.collectFirst{
        case bp@SecondOrderPoint(point) if range.contains(point) => bp
      }.map(SecondOrderBreakPointError.apply).toLeft(
        ((pow(range.length, 5) * abs(derivative.maxValue(range))) /
            (abs(accuracy) * 180.0) + 1.0).toInt
      )
    ).map(n => if (n < 3) 3 else if (n > 10_000_000) 10_000_000 else n).getOrElse(3)
}
