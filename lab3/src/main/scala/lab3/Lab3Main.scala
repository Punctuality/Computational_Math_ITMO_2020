package lab3

import lab3.input.FunctionParser
import lab3.input.ui.{ControllerFrame, MultipleEquationFrame}

object Lab3Main extends App with FunctionParser {

  val frame = new ControllerFrame()

  frame.setVisible(true)

}
