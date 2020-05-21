package lab3.input.ui

import java.awt.{FlowLayout, GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JFrame}

class ControllerFrame extends JFrame {

  private val oneEqFrame = new OneEquationFrame
  private val twoEqFrame = new MultipleEquationFrame

  this.setLayout(new GridBagLayout())
  private val oneEqButton = new JButton("One Equation Solver")
  private val twoEqButton = new JButton("Multiple Equation Solver")
  private val constr = new GridBagConstraints()

  constr.fill = GridBagConstraints.BOTH
  constr.gridx = 0
  constr.gridy = 0
  constr.weightx = 0.4
  this.add(oneEqButton)

  constr.gridx = 1
  constr.gridy = 0
  constr.weightx = 0.4
  this.add(twoEqButton)

  oneEqButton.addActionListener{_ => oneEqFrame.setVisible(true)}
  twoEqButton.addActionListener{_ => twoEqFrame.setVisible(true)}

  this.setSize(500, 100)
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
}
