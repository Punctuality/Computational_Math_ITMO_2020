package lab4.input.ui

import java.awt.{GridBagConstraints, GridBagLayout, GridLayout}

import cats.{Eval, Id}
import core.input.FunctionParser
import core.input.ui.components.{ChoiceGroup, JGraph, TextEntryField}
import core.math.FromDouble.Instances._
import javafx.application.Platform
import javax.swing._
import lab4.algorithm.{Interpolator, SplineInterpolator}

import scala.math.round

class InterpolatorFrame extends JFrame with FunctionParser {

  private val solvers: Map[String, Interpolator[Double, Double, Eval]] = Map(
    "Spline Interpolation" -> new SplineInterpolator[Double, Eval]
  )

  private val function = new TextEntryField("Function", "sin(x)", false)
  private val leftBound = new TextEntryField("Left Bound", "-10", false)
  private val rightBound = new TextEntryField("Right Bound", "10", false)
  private val dotsNumber = new TextEntryField("Dots Number", "6", false)
  private val testXCoord = new TextEntryField("Test Dot (X coordinate)", "", false)

  private val algorithmPicker = new ChoiceGroup(solvers.keys.toList, 4, "")

  private val graphArea = new JGraph("lab4/css/lineChart.css").init(610, 500)

  private val interpolationButton = new JButton("INTERPOLATE")
  private val predictButton = new JButton("Predict")

  interpolationButton.addActionListener(_ => drawInterpolation(false))
  predictButton.addActionListener(_ => if(testXCoord.getValue.nonEmpty) drawInterpolation(true))

  private val valuesPanel = new JPanel(new GridLayout(5, 1))
  List(function, leftBound, rightBound, dotsNumber, testXCoord).foreach(valuesPanel.add)

  private val buttonPanel = new JPanel(new GridLayout(2, 1))
  List(interpolationButton, predictButton).foreach(buttonPanel.add)

  this.setLayout(new GridBagLayout())

  private val constraints = new GridBagConstraints()
  constraints.fill = GridBagConstraints.BOTH

  constraints.gridx = 0; constraints.gridy = 0; constraints.weightx = 0.50; constraints.weighty = 0.80
  this.add(graphArea, constraints)

  constraints.gridx = 1; constraints.gridy = 0; constraints.weightx = 0.50; constraints.weighty = 0.80
  this.add(valuesPanel, constraints)

  constraints.gridx = 0; constraints.gridy = 1; constraints.weightx = 0.50; constraints.weighty = 0.20
  this.add(algorithmPicker, constraints)

  constraints.gridx = 1; constraints.gridy = 1; constraints.weightx = 0.50; constraints.weighty = 0.20
  this.add(buttonPanel, constraints)

  this.setSize(1000, 600)
  this.setResizable(false)
  this.setVisible(false)
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  Platform.runLater(() => drawInterpolation(false))

  private def drawInterpolation(testDot: Boolean): Unit = {
    val func: Double => Double = parseExpressionByX[Double, Double, Id](function.getValue)

    (
      leftBound.getValue.toDoubleOption,
      rightBound.getValue.toDoubleOption,
      dotsNumber.getValue.toDoubleOption,
      algorithmPicker.getSelectedChoice,
    ) match {
      case (Some(left), Some(right), Some(n), Some((_ ,algoName))) if left < right =>
        val origX: LazyList[Double] = LazyList.iterate(left)(_ + (right - left) / 100.0).takeWhile(_ <= (right + 1e-3))
        val origY: LazyList[Double] = origX.map(func)

        val solver: Interpolator[Double, Double, Eval] = solvers(algoName)

        val tableX = LazyList.iterate(left)(_ + (right - left) / (n - 1.0)).takeWhile(_ <= (right + 1e-3))
        val tableY = tableX.map(func)

        val interpolatedFunc: Option[Double => Double] = solver.interpolate(tableX.zip(tableY).toList).value
        val interpolatedX: LazyList[Double] = if (testDot) {
          val testX = testXCoord.getValue.toDoubleOption
          val interLeft = testX.filter(_ < left).getOrElse(left)
          val interRight = testX.filter(_ > right).getOrElse(right)
          LazyList.iterate(interLeft)(_ + (interRight - interLeft) / 100.0).takeWhile(_ <= (interRight + 1e-3))
        } else {
          origX
        }

        val rounding: Double => Double = a => round(a * 100.0) / 100.0

        val origData = Some(origX.zip(origY).toList)
          .map(data => "Original Func" -> data)
        val tableXData = Some(tableX.zip(tableY).toList)
          .map(data => "Interpolation Dots" -> data)
        val interpolatedData = interpolatedFunc.map(f => interpolatedX.zip(interpolatedX.map(f)).toList)
          .map(data => "Interpolated Func" -> data)
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
              origData,
              interpolatedData,
              tableXData,
              testDotData
            ).flatten.toMap)
        }

      case _ => ()
    }
  }
}
