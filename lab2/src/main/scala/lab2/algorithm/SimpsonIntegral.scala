package lab2.algorithm

import lab2.math._

import scala.math._

class SimpsonIntegral(function: DefinedFunction[Double]) {

  private val eps = 0.00001

  def result(range: DefinedRange[Double], accuracy: Double): Either[IntegrateError, Double] = for {
    nSections <- countSections(range, accuracy)
    _ = println(s"Sections: $nSections")
    result <- integrate(range, nSections)
  } yield result

  def integrate(range: DefinedRange[Double], nSections: Int): Either[IntegrateError, Double] =
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
          val xs = LazyList.iterate(start)(_ + step).take(nSections - nSections % 2 + 1).iterator
          Right apply xs.sliding(3, 2).map(_.map(function).toList).map{
            case yPrev :: yMid :: yNext :: Nil => yPrev + (4 * yMid) + yNext
          }.sum * step / 3.0
        case ranges => ranges.map(integrate(_, nSections)).reduce((a, b) => a.flatMap(aVal => b.map(aVal + _)))
      }
    }.joinRight

  def countSections(range: DefinedRange[Double], accuracy: Double): Either[IntegrateError, Int] =
    function.derivatives.get(4).toRight(
      NoProperDerivative(4)
    ).map(derivative =>
      (
        (pow(range.length, 5) * abs(derivative.maxValue(range))) /
          (abs(accuracy) * 180) + 1
      ).toInt
    )
}
