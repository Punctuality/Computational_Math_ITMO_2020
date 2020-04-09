package lab2.input

import java.util.concurrent.atomic.AtomicBoolean

import lab2.algorithm._
import lab2.math.{DefinedRange, SecondOrderPoint}

import scala.io.StdIn._

class ConsoleClient{
  val endFlag = new AtomicBoolean(false)

  private def help(): Unit =
    println(
      """
        |Available commands:
        | - help
        | - integrate
        | - end
        |""".stripMargin
    )

  private def integrate(): Unit = {
    val functions = PresetFunctions.notationsToFunctions
    println(
      """Choose one of the presented methods:
        | 1. Simpson's rule
        |""".stripMargin)
    readLine().trim match {
      case "1" =>
        println(s"Choose one of presented functions:\n${
          functions.map(_._1).zipWithIndex.map{case (notation, index) => s" ${index + 1}. $notation"}.mkString("\n")
        }")
        readLine()
          .toIntOption
          .filter(ind => ind >= 1 && ind <= functions.length)
          .map(ind => functions.apply(ind - 1)._2) match {
          case Some(func) =>
            val integrator = new SimpsonIntegral(func)
            println("Enter 'left_bound', 'right_bound', 'accuracy' (separate them with space):")
            readLine().trim.split(' ').flatMap(_.toDoubleOption).toList match {
              case start :: stop :: accuracy :: Nil =>
                integrator.result(DefinedRange(start, stop), accuracy) match {
                  case Right((result, error, n)) => println(
                    s"""
                      |Resulted integral equal: $result
                      |Approx error:            $error
                      |N Sections:              $n
                      |""".stripMargin)
                  case Left(OutOfRangeBounds(DefinedRange(start, stop))) =>
                    println(
                      s"""You've set range which exceeds allowable range for this function:
                        |Yours:     $start -> $stop
                        |Allowable: ${func.definedOnRange.map{case DefinedRange(start, stop) => s"$start -> $stop"}.mkString(" | ")}
                        |""".stripMargin)
                  case Left(NoProperDerivative(4)) =>
                    println("Provided func doesn't have 4th order derivative.")
                  case Left(SecondOrderBreakPointError(SecondOrderPoint(point))) =>
                    println(
                      s"""You've set range which contains Second Order Break Point for current function:
                        |Range: $start -> $stop
                        |Point: $point
                        |""".stripMargin)
                }
              case _ => println("No match")
            }
          case None => println("No match")
        }
      case _   => println("No match")
    }
  }

  private def end(): Unit = endFlag.set(true)

  def startAskingLoop(): Unit = {
    print("Enter on of available commands: ")
    readLine().trim.toLowerCase match {
      case "help" => help()
      case "integrate" => integrate()
      case "end" => end()
      case _ => help()
    }
    if (endFlag.get()) () else startAskingLoop()
  }

  help()
}