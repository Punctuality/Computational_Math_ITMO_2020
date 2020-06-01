package lab4

import cats.Id
import core.math.FromDouble.Instances._
import lab4.algorithm.SplineInterpolator

import scala.math._

object Lab4Main extends App {
  val solver = new SplineInterpolator[Double, Id]

  val dots = List(
    -3.99 ->  0.75, // C
    -2.67 -> -0.45, // D
    -1.17 -> -0.92, // E
     0.51 ->  0.49, // F
     2.21 ->  0.80, // G
     3.23 -> -0.09, // H
     4.03 -> -0.78  // I
  )

  val func = solver.interpolate(dots).get

  val ys_o = dots.map(a => a._2).map(v => round(v * 100.0) / 100.0)
  val fs_o = dots.map(a => func(a._1)).map(v => round(v * 100.0) / 100.0)
  val diff_o = (fs_o zip ys_o).map{ case (d, d1) => abs(d - d1)}.map(v => round(v * 1000.0) / 1000.0)

  println("Original")

  println(dots.map(_._1).mkString("\t"))
  println(ys_o.mkString("\t"))
  println(fs_o.mkString("\t"))
  println(diff_o.mkString("\t"))

  println("Spane")

  val xs = LazyList.iterate(-4.0)(_ + 0.2).takeWhile(_ <= 4.0).force.toList.map(v => round(v * 100.0) / 100.0)
  val ys = xs.map(sin).map(v => round(v * 100.0) / 100.0)
  val fs = xs.map(func).map(v => round(v * 100.0) / 100.0)
  val diff = (fs zip ys).map{ case (d, d1) => abs(d - d1)}.map(v => round(v * 1000.0) / 1000.0)

  println(xs.mkString("\t"))
  println(ys.mkString("\t"))
  println(fs.mkString("\t"))
  println(diff.mkString("\t"))
}
