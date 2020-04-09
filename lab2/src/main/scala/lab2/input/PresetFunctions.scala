package lab2.input

import lab2.math.{DefinedFunction, DefinedRange, FirstOrderPoint, SecondOrderPoint}

import scala.math._

object PresetFunctions{
  private val linear: DefinedFunction[Double] = DefinedFunction(
    x => x,
    Map(
      4 -> DefinedFunction(
        _ => 0,
        Map.empty,
        List(DefinedRange(Double.MinValue, Double.MaxValue)),
        List.empty
      )
    ),
    List(DefinedRange(Double.MinValue, Double.MaxValue)),
    List.empty
  )

  private val squareRoot: DefinedFunction[Double] = DefinedFunction(
    x => sqrt(x),
    Map(
      4 -> DefinedFunction(
        x => -15.0 / 16.0 * pow(x, -7.0/2.0),
        Map.empty,
        List(DefinedRange(Double.MinValue, Double.MaxValue)),
        List(SecondOrderPoint(0))
      )
    ),
    List(DefinedRange(0, Double.MaxValue)),
    List.empty
  )

  private val forthDegree: DefinedFunction[Double] = DefinedFunction(
    x => 120 * pow(x, 4) + x - 10,
    Map(4 -> DefinedFunction(
      _ => 2880,
      Map.empty,
      List(DefinedRange(Double.MinValue, Double.MaxValue)),
      List.empty
    )),
    List(DefinedRange(Double.MinValue, Double.MaxValue)),
    List.empty
  )

  private val hyperbola: DefinedFunction[Double] = DefinedFunction(
    x => 1.0 / (10.0 * x),
    Map(4 -> DefinedFunction(
      x => 12.0 / 5.0 * pow(x, -5),
      Map.empty,
      List(DefinedRange(Double.MinValue, Double.MaxValue)),
      List(SecondOrderPoint(0))
    )),
    List(DefinedRange(Double.MinValue, Double.MaxValue)),
    List(SecondOrderPoint(0))
  )

  private val signum: DefinedFunction[Double] = DefinedFunction(
    x => copySign(1.0, x),
    Map(4 -> DefinedFunction(
      _ => 0,
      Map.empty,
      List(DefinedRange(Double.MinValue, Double.MaxValue)),
      List(FirstOrderPoint(0))
    )),
    List(DefinedRange(Double.MinValue, Double.MaxValue)),
    List(FirstOrderPoint(0))
  )


  private val breakPiPoints = Range(-1000, 1000).map(k => (Pi * k.toDouble) + Pi / 2.0).map(SecondOrderPoint(_)).toList
  private val tanOverLine: DefinedFunction[Double] = DefinedFunction(
    x => tan(x) / x,
    Map(4 -> DefinedFunction(
      x => 24.0 * tan(x) / pow(x, 5) - 24.0 * pow(cos(x), -2) / pow(x, 4) + 24.0 * tan(x) * pow(cos(x), -2) / pow(x, 3) -
        4.0 * (2 * pow(cos(x), -4) + 4.0 * pow(tan(x), 2) * pow(cos(x), -2)) / pow(x, 2) +
        (16.0 * tan(x) * pow(cos(x), -4) + 8.0 * pow(tan(x), 3) * pow(cos(x), -2)) / x,
      Map.empty,
      List(DefinedRange(Double.MinValue, Double.MaxValue)),
      breakPiPoints
    )),
    List(DefinedRange(Double.MinValue, Double.MaxValue)),
    breakPiPoints
  )

  val notationsToFunctions: List[(String, DefinedFunction[Double])] = List(
    "y = x" -> linear,
    "y = √x" -> squareRoot,
    "y = 120x^4 + x - 10" -> forthDegree,
    "y = 1 / 10x" -> hyperbola,
    "y = sign(x)" -> signum,
    "y = tan(x) / x" -> tanOverLine
  )
}
