package lab3.input.ui.components

import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing._

class TextEntryField(text: String, standardValue: String = "", readOnly: Boolean, minText: Int = 20) extends JPanel {
  private val textNotationElem = new JLabel(text)
  private val textEntryElem = new JTextField(standardValue)

  textNotationElem.setText(text + (0 until (minText-text.length)).toList.map(_ => ' ').mkString(""))

  textEntryElem.setEditable(!readOnly)

  this.setLayout(new GridBagLayout())

  private val c = new GridBagConstraints
  c.fill = GridBagConstraints.BOTH

  c.weightx = 0.3
  c.gridx = 0
  c.gridy = 0
  this.add(textNotationElem, c)

  c.weightx = 0.7
  c.gridx = 1
  c.gridy = 0
  this.add(textEntryElem, c)
  this.setVisible(true)

  def getValue: String = textEntryElem.getText
  def setValue(value: String): Unit = textEntryElem.setText(value)
}
