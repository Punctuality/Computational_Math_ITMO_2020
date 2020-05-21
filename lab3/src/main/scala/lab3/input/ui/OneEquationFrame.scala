package lab3.input.ui

import java.awt.{GridBagConstraints, GridBagLayout, GridLayout}

import cats.Eval
import javafx.application.Platform
import javax.swing.{JButton, JFrame, JPanel}
import lab3.algorithm.separator.TabularSeparator
import lab3.algorithm.solver.{ChordSolver, NewtonSolver}
import lab3.input.FunctionParser
import lab3.input.ui.components.{ChoiceGroup, JGraph, TextEntryField}
import lab3.math.FromDouble.Instances._

import scala.math._

class OneEquationFrame extends JFrame with FunctionParser {
  private val graph: JGraph = new JGraph("lab3/css/lineChart.css").init(800 -> 400)
  private val methodChoice: ChoiceGroup = new ChoiceGroup(List("Chord", "Tangent"), 1, "")

  private val funcField = new TextEntryField("F(x) = ", "sin(x)", false)
  private val iterField = new TextEntryField("iterations = ", "1000", false)
  private val resXField = new TextEntryField("Result X = ","?" , false)
  private val resYField = new TextEntryField("Result Y = ", "?", false)

  private val leftField = new TextEntryField("Left  Bound = ", "0", false)
  private val rightField= new TextEntryField("Right Bound = ", "4", false)
  private val accField  = new TextEntryField("Accuracy = ", "0.01", false)
  private val errorField= new TextEntryField("Error = ", "?", true)

  private val valuesPanel = new JPanel(new GridLayout(4, 2))
  List(funcField, leftField, iterField, rightField, resXField, accField, resYField, errorField).foreach(
    field => valuesPanel.add(field)
  )

  private val calcButton = new JButton("CALC")
  private val exitButton = new JButton("CLOSE")
  private val buttonPanel = new JPanel(new GridLayout(2, 1))
  buttonPanel.add(calcButton)
  buttonPanel.add(exitButton)

  calcButton.addActionListener { _ =>
    calculate()
  }

  exitButton.addActionListener {_ =>
    this.setVisible(false)
  }

  private val constraints = new GridBagConstraints()
  constraints.fill = GridBagConstraints.BOTH
  this.setLayout(new GridBagLayout())

  constraints.gridx = 0; constraints.gridy = 0; constraints.weightx = 0.8
  this.add(graph, constraints)

  constraints.gridx = 1; constraints.gridy = 0; constraints.weightx = 0.2
  this.add(methodChoice, constraints)

  constraints.gridx = 0; constraints.gridy = 1; constraints.weightx = 0.8
  this.add(valuesPanel, constraints)

  constraints.gridx = 1; constraints.gridy = 1; constraints.weightx = 0.2
  this.add(buttonPanel, constraints)

  this.setSize(1000, 600)
  this.setResizable(false)
  this.setVisible(false)


  def calculate(): Unit = {

    val acc = accField.getValue.toDouble
    val iterations = if (iterField.getValue.isEmpty ||iterField.getValue.toInt == 0) 1000 else iterField.getValue.toInt

    val (left, right) = (leftField.getValue.toDouble, rightField.getValue.toDouble)

    val separator = new TabularSeparator[Double, Double, Eval]((right - left) / iterations)
    val solver = methodChoice.getSelectedChoice match {
      case Some((0, _)) => new ChordSolver[Double, Eval](acc, iterations, 20)
      case Some((1, _)) => new NewtonSolver[Double, Eval](acc, iterations, 20)
    }

    val func = parseExpressionByX[Double, Double, Eval](funcField.getValue).andThen(_.value)

    val bounds: List[(Double, Double)] = separator.separateSolutions(func, left, right).value.map{
      case ((x1, _), (x2, _)) => x1 -> x2
    }


    val solutions: List[(Int, Double, Double)] = bounds.flatMap {
      case (lBound, rBound) => solver.findSolution(func, lBound, rBound).value
    }


    val rounding: Double => Double = a => round(a * pow(acc, -1)) / pow(acc, -1)

    iterField.setValue(solutions.map(_._1).sum.toString)
    resXField.setValue(solutions.map(_._2).map(rounding).mkString(" "))
    resYField.setValue(solutions.map(_._2).map(func).map(rounding).mkString(" "))
    errorField.setValue(solutions.map(r => abs(r._3 - r._2)).map(rounding).mkString(" "))

    val Xs: List[Double] = LazyList.iterate(left)(x => (right-left) / 500.0 + x).takeWhile(_ < right).toList

    Platform.runLater{
      () =>
        graph.removeData()
        graph.setLinesData(Map("LINE" -> Xs.map(x => x -> func(x))))
        graph.setLinesData(Map("SOLUTIONS" -> solutions.map(_._2).map(x => x -> func(x))))
    }
  }

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

}
