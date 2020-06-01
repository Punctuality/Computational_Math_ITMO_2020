package core.input.ui.components

import java.awt.{GridBagConstraints, GridBagLayout, GridLayout}

import javax.swing._

import scala.math.min

class ChoiceGroup(choices: List[String], horizontalMax: Int, notation: String) extends JPanel{
  this.setVisible(true)

  this.setLayout(new GridLayout(choices.length / horizontalMax + 1, min(choices.length, horizontalMax)))

  private val buttonGroup = new ButtonGroup()

  private val radioButtons: List[JPanel] = choices.map { choice =>
    val button = new JRadioButton(choice)
    val label = new JLabel(notation)

    buttonGroup.add(button)

    val subPanel = new JPanel(new GridBagLayout)
    val c = new GridBagConstraints()
    c.fill = GridBagConstraints.HORIZONTAL

    c.gridx = 0
    c.gridy = 0
    c.weightx = choice.length.toDouble / (choice.length + notation.length).toDouble
    subPanel.add(button, c)

    c.gridx = 1
    c.gridy = 0
    c.weightx = notation.length.toDouble / (choice.length + notation.length).toDouble
    subPanel.add(label, c)

    subPanel
  }

  radioButtons.foreach(this.add)

  if (buttonGroup.getElements.hasMoreElements){
    buttonGroup.getElements.nextElement().setSelected(true)
  }


  def getSelectedChoice: Option[(Int, String)] = {
    var selected: Option[(Int, String)] = None
    val elems = buttonGroup.getElements

    while(elems.hasMoreElements && selected.isEmpty) {
      val button = elems.nextElement()
      if (button.isSelected) {
        val text = button.getText
        val index = choices.indexOf(text)
        selected = Some(index -> text)
      }
    }

    selected
  }

}
