package lab2

import lab2.algorithm.SimpsonIntegral
import lab2.math.{DefinedFunction, DefinedRange}

import scala.math._

object Lab2Main extends App {
  val simpsonIntegral = new SimpsonIntegral(
    DefinedFunction(
      x => sin(1 / x),
      derivatives = Map(
        4 -> DefinedFunction(
          x => (pow(x, -8) - 36 * pow(x, -6)) * sin(1/x) + (24 * pow(x, -5) - 12 * pow(x, -7)) * cos(1/x),
          Map.empty,
          List(DefinedRange(Int.MinValue, Int.MaxValue)),
          List.empty
        )
      ),
      List(DefinedRange(Int.MinValue, Int.MaxValue)),
      List.empty
    )
  )

  println(simpsonIntegral.result(DefinedRange(2, 5), 0.0001))
}
