package lab5.input.ui

import java.awt.{GridBagConstraints, GridBagLayout, GridLayout}

import cats.{Eval, Id}
import core.input.FunctionParser
import core.input.ui.components.{ChoiceGroup, JGraph, TextEntryField}
import core.math.FromDouble.Instances._
import javafx.application.Platform
import javax.swing._
import lab4.algorithm.SplineInterpolator
import lab5.algorithm.{AdamsODESolver, ODESolver, RungeKuttaODESolver}
import lab5.math.{CustomDifferentialEquation, OrdinaryDifferentialEquation}

import scala.math._

class SolverFrame extends JFrame with FunctionParser {

  def getCount(range: Double, accuracy: Double, accOrder: Double): Int = {
    val res = (range / pow(accuracy, 1.0d / accOrder) + (1.0d - 1e-9d)).toInt
    max(10, res)
  }

  private val interpolator = new SplineInterpolator[Double, Eval]

  private val funcParser = new CustomDifferentialEquation[Double, Id]

  private val solvers: Map[String, (ODESolver[Double, Eval, Id], Int)] = Map(
    "Adams-Bashforth" -> (new AdamsODESolver[Double, Eval](0.1) -> 5),
    "Runge-Kutta" -> (new RungeKuttaODESolver[Double, Eval], 4)
  )

  private val function = new TextEntryField("Function", "x", false)
  private val leftBound = new TextEntryField("Left Bound", "0", false)
  private val rightBound = new TextEntryField("Right Bound", "0", false)
  private val leftBoundValue = new TextEntryField("Left Bound Val", "0", false)
  private val accuracy = new TextEntryField("Accuracy", "0.1", false)

  private val testXCoord = new TextEntryField("Test Dot (X coordinate)", "", false)

  private val algorithmPicker = new ChoiceGroup(solvers.keys.toList, 2, "")

  private val graphArea = new JGraph("lab5/css/lineChart.css").init(650, 450)

  private val solveButton = new JButton("SOLVE")
  private val predictButton = new JButton("PREDICT")

  solveButton.addActionListener(_ => drawInterpolation(false))
  predictButton.addActionListener(_ => if(testXCoord.getValue.nonEmpty) drawInterpolation(true))

  private val valuesPanel = new JPanel(new GridLayout(5, 1))
  List(function, leftBound, rightBound, leftBoundValue, accuracy).foreach(valuesPanel.add)

  private val buttonPanel = new JPanel(new GridLayout(3, 1))
  List(solveButton, predictButton, testXCoord).foreach(buttonPanel.add)

  this.setLayout(new GridBagLayout())

  private val constraints = new GridBagConstraints()
  constraints.fill = GridBagConstraints.BOTH

  constraints.gridx = 0; constraints.gridy = 0; constraints.weightx = 0.50; constraints.weighty = 0.80
  this.add(graphArea, constraints)

  constraints.gridx = 1; constraints.gridy = 0; constraints.weightx = 0.50; constraints.weighty = 0.60
  this.add(valuesPanel, constraints)

  constraints.gridx = 0; constraints.gridy = 1; constraints.weightx = 0.50; constraints.weighty = 0.20
  this.add(algorithmPicker, constraints)

  constraints.gridx = 1; constraints.gridy = 1; constraints.weightx = 0.50; constraints.weighty = 0.40
  this.add(buttonPanel, constraints)

  this.setSize(1000, 600)
  this.setResizable(false)
  this.setVisible(false)
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  Platform.runLater(() => drawInterpolation(false))

  private def drawInterpolation(testDot: Boolean): Unit = {
    (
      leftBound.getValue.toDoubleOption,
      rightBound.getValue.toDoubleOption,
      leftBoundValue.getValue.toDoubleOption,
      accuracy.getValue.toDoubleOption,
      algorithmPicker.getSelectedChoice,
      function.getValue.nonEmpty
    ) match {
      case (Some(left), Some(right), Some(leftVal), Some(acc), Some((_, algoName)), true) if left < right =>
        val (solver: ODESolver[Double, Eval, Id], accOrder: Int) = solvers(algoName)

        val range: Double = right - left
        val count: Int    = getCount(range, acc, accOrder)
        val step: Double  = range / (count - 1)

        println(s"Current Count: $count")

        val rounding: Double => Double = a => round(a * 100.0) / 100.0
        val func: OrdinaryDifferentialEquation[Double, Id] = funcParser.getODE(function.getValue)

        if (count < 100) {
          val resultedPoints: List[(Double, Double)] = solver.solveODE(func, (left, leftVal), step, count).value

          val interpolatedFunc: Option[Double => Double] = interpolator.interpolate(resultedPoints).value

          val interpolatedPoints: List[(Double, Double)] = (if (testDot) {
            val testX = testXCoord.getValue.toDoubleOption
            val interLeft = testX.filter(_ < left).getOrElse(left)
            val interRight = testX.filter(_ > right).getOrElse(right)
            LazyList.iterate(interLeft)(_ + (interRight - interLeft) / 100.0).takeWhile(_ <= (interRight + 1e-3))
          } else {
            LazyList.iterate(left)(_ + range / 100.0).takeWhile(_ <= (right + 1e-3))
          }).flatMap(x => interpolatedFunc.map(f => x -> f(x))).toList

          val resultData = Some(resultedPoints)
            .map(data => "Result Dots" -> data)
          val interpolatedData = Some(interpolatedPoints)
            .map(data => "Result Func (Interpolated)" -> data)
          val testDotData = Option(testDot).filter(identity)
            .flatMap(_ => testXCoord.getValue.toDoubleOption)
            .flatMap(x => interpolatedFunc.map(f => x -> f(x)))
            .map { dot =>
              val niceDot = rounding(dot._1) -> rounding(dot._2)
              s"Test Dot $niceDot" -> List(dot)
            }

          Platform.runLater{
            () =>
              graphArea.removeData()
              graphArea.setLinesData(List(
                interpolatedData,
                resultData,
                testDotData
              ).flatten.toMap)
          }
        } else {
          ()
        }
      case _ => ()
    }
  }
}
