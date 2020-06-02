package lab3

import core.input.FunctionParser
import lab3.input.ui.ControllerFrame

object Lab3Main extends App with FunctionParser {

  val frame = new ControllerFrame()

  frame.setVisible(true)

}
