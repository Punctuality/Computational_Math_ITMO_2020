import java.io.File
import java.util.concurrent.atomic.AtomicReference

import algorithm.Gauss
import input.{ConsoleInput, FileInput, RandomInput}
import math.Matrix

import scala.io.StdIn.readLine
import scala.math._

object Lab1Main extends App {
  val retrievedMatrix = new AtomicReference[Option[Matrix[Double]]](None)

  println(
    """
      |Choose which matrix to use:
      |1. Input matrix values in console
      |2. Input matrix values by passing file
      |3. Generate random matrix
      |""".stripMargin)
  readLine match {
    case "1" =>
      retrievedMatrix set ConsoleInput.doubleConsole().produceMatrix
    case "2" =>
      println(s"Input file path:")
      retrievedMatrix set FileInput.doubleFile(new File(readLine())).produceMatrix
    case "3" =>
      println(s"Input dimensions and max value (rows cols max_value):")
      readLine.split(' ').map(_.toInt).toList match {
        case m :: n :: maxValue :: Nil => retrievedMatrix set RandomInput.intRandom(m, n, maxValue).produceMatrix.map(_.map(_.toDouble))
        case _             => ()
      }
    case _   => ()
  }

  retrievedMatrix.get match {
    case Some(matrix) => Gauss.solveSystem(matrix) match {
      case Left(message) => println(message)
      case Right(answers) =>
        println(s"For matrix:\n$matrix")
        println("Result:")
        answers.foreach{ case (pos, value) => println(s"x_${pos+1} = ${round(value * 100).toDouble / 100}")}
    }
    case None         => println("No valid option or matrix was provided")
  }
}
