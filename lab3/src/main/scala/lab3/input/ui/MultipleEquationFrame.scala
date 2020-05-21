package lab3.input.ui

import java.awt.{GridBagConstraints, GridBagLayout, GridLayout}

import cats.Eval
import javafx.application.Platform
import javax.swing.{JButton, JFrame, JPanel}
import lab3.algorithm.multisolver.NewtonMultiSolver
import lab3.input.FunctionParser
import lab3.input.ui.components.{ChoiceGroup, JGraph, TextEntryField}
import lab3.math.FromDouble.Instances._

import scala.math._

class MultipleEquationFrame extends JFrame with FunctionParser {

  // Graph | Values
  // Funcs | Calc

  // Graph
  private val graph: JGraph = new JGraph("lab3/css/lineChartMulty.css").init(700 -> 400)

  // Values
  private val iterField = new TextEntryField("iterations = ", "1000", false)
  private val resXField = new TextEntryField("Result X = ","?" , false)
  private val resYField = new TextEntryField("Result Y = ", "?", false)
  private val startXField = new TextEntryField("Start X = ", "0", false)
  private val startYField= new TextEntryField("Start Y = ", "4", false)
  private val accField  = new TextEntryField("Accuracy = ", "0.01", false)

  private val valuesPanel = new JPanel(new GridLayout(6, 1))
  List(iterField, startXField, startYField, accField, resXField, resYField).foreach(
    field => valuesPanel.add(field)
  )

  // Funcs
  private val funcExprs: List[(String, String)] = List(
    "x - y" -> "x",
    "x^2 - 2*x + y - 3" -> "-x^2 + 2*x+ 3",
    "-2*x^2 - 3*x + y + 4" -> "2*x^2 + 3*x - 4",
    "y - 2 * e^x + 1" -> "2 * e^x - 1"
  )

  private val functions: List[((Double, Double) => Double, Double => Double)] = funcExprs.map{
      case (twoVarFunc, oneVarFunc) =>
        ((x: Double, y: Double) =>
          parseExpression[Double, Double, Eval](twoVarFunc)(Map("x" -> x, "y" -> y)).value) ->
          parseExpressionByX[Double, Double, Eval](oneVarFunc).andThen(_.value)
  }

  private val firstEq = new ChoiceGroup(funcExprs.map(_._2), 1, "= y")
  private val secondEq = new ChoiceGroup(funcExprs.map(_._2), 1, "= y")
  private val funcsPanel = new JPanel(new GridLayout(1, 2))
  funcsPanel.add(firstEq)
  funcsPanel.add(secondEq)
  // Calc

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

  constraints.gridx = 0; constraints.gridy = 0; constraints.weightx = 0.7
  this.add(graph, constraints)

  constraints.gridx = 1; constraints.gridy = 0; constraints.weightx = 0.3
  this.add(valuesPanel, constraints)

  constraints.gridx = 0; constraints.gridy = 1; constraints.weightx = 0.6
  this.add(funcsPanel, constraints)

  constraints.gridx = 1; constraints.gridy = 1; constraints.weightx = 0.4
  this.add(buttonPanel, constraints)

  this.setSize(1300, 600)
  this.setResizable(false)
  this.setVisible(false)


  def calculate(): Unit = {
    val acc = accField.getValue.toDouble
    val iterations = if (iterField.getValue.isEmpty || iterField.getValue.toInt == 0) 1000 else if (iterField.getValue.toInt < 21) 21 else iterField.getValue.toInt

    val (startX, startY) = (startXField.getValue.toDouble, startYField.getValue.toDouble)

    if (firstEq.getSelectedChoice.get._1 != secondEq.getSelectedChoice.get._1){
      val func1: ((Double, Double) => Double, Double => Double) = functions(firstEq.getSelectedChoice.get._1)
      val func2: ((Double, Double) => Double, Double => Double) = functions(secondEq.getSelectedChoice.get._1)

      val solver = new NewtonMultiSolver[Double, Eval](acc, iterations)

      val funcForSolve: List[Double] => List[Double] = {
        case x1 :: x2 :: Nil => func1._1(x1, x2) :: func2._1(x1, x2) :: Nil
      }

      solver.findSolution(funcForSolve, startX :: startY :: Nil).value match {
        case None =>
          resXField.setValue("No Solution")
          resYField.setValue("No Solution")
        case Some((finalIter, resX :: resY :: Nil)) =>
          val rounding: Double => Double = a => round(a * pow(acc, -1)) / pow(acc, -1)

          iterField.setValue(finalIter.toString)
          resXField.setValue(rounding(resX).toString)
          resYField.setValue(rounding(resY).toString)

          val Xs: List[Double] = LazyList.iterate(startX)(x => (resX - startX) / 250.0 + x).takeWhile(_ < resX * 2 - startX).toList

          Platform.runLater {
            () =>
              graph.removeData()
              graph.setLinesData(Map("LINE_1" -> Xs.map(x => x -> func1._2(x))))
              graph.setLinesData(Map("LINE_2" -> Xs.map(x => x -> func2._2(x))))
              graph.setDotsData(Map("SOLUTION" -> (resX, resY)))
          }
      }
    }
  }

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

}
