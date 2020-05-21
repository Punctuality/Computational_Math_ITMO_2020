package lab3

import cats.{Eval, Id}
import cats.effect.IO
import cats.implicits._
import lab3.algorithm.separator.TabularSeparator
import lab3.algorithm.solver.{HordSolver, NewtonSolver}
import lab3.input.FunctionParser
import lab3.input.ui.OneEquationFrame
import lab3.math.FromDouble.Instances._

import scala.math._
import scala.math.Fractional.Implicits._

object Lab3Main extends App with FunctionParser {

//  val startTime = System.currentTimeMillis()
//
//  val separator = TabularSeparator[Double, Double, IO](0.00001)
//
//  val testFunc: Double => Double = sin
//
//  separator.separateSolutions(testFunc, -0.009, 2 * Pi+0.009).unsafeRunAsync {
//    case Right(res) =>
//      val stopTime = System.currentTimeMillis()
//      println(res.mkString("\n"))
//      println(stopTime - startTime)
//
//    case Left(ex) => ex.printStackTrace()
//  }
//
//  println("Started?!")

//  val testFunc: Double => Double = parseExpressionByX[Double, Double, Id]("sin(x)")
//
//  val solver = HordSolver[Double, Eval](0.01, 1e5.toInt, 10)
//
//  val solution = solver.findSolution(testFunc, -10, 10).value
//
//  println(solution)

  val frame = new OneEquationFrame()

  frame.setVisible(true)

}
