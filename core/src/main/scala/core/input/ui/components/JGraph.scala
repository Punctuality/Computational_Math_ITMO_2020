package core.input.ui.components

import java.awt.{Color, Dimension}

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.scene.chart.{LineChart, NumberAxis, XYChart}
import javafx.scene.{Group, Scene}
import javax.swing._

class JGraph(stylePath: String) extends JPanel{

  lazy val xAxis = new NumberAxis
  lazy val yAxis = new NumberAxis
  lazy val lineChart = new LineChart[Number, Number](xAxis, yAxis)

  lazy val jfxRoot = new Group(lineChart)
  lazy val jfxScene = new Scene(jfxRoot)
  lazy val jfxPanel = new JFXPanel()

  def init(size: (Int, Int)): JGraph = {

    jfxPanel.setScene(jfxScene)
    this.add(jfxPanel)
    this.setVisible(isVisible)
    this.setBackground(Color.GRAY)

    val sharedDim = new Dimension(size._1, size._2)
    jfxRoot.setAutoSizeChildren(true)
    jfxPanel.setPreferredSize(sharedDim)
    lineChart.setPrefSize(sharedDim.width, sharedDim.height)
    this.setSize(sharedDim)

    Platform.runLater(() => jfxScene.getStylesheets.add(stylePath))

    this
  }

  def removeData(): Unit = Platform.runLater{ () =>
    lineChart.getData.clear()
    jfxScene.getStylesheets.add(stylePath)
  }

  private def addLineDataToSeries(series: XYChart.Series[Number, Number], data: List[(Double, Double)]): Unit =
    data.foreach{ case (x, y) => series.getData.add(new XYChart.Data(x, y)) }

  def setLinesData(data: Map[String, List[(Double, Double)]]): Unit = Platform.runLater{() =>
    data.foreach{
      case (name, pointsData) =>
        val series = new XYChart.Series[Number, Number]
        series.setName(name)
        addLineDataToSeries(series, pointsData)
        lineChart.getData.add(series)
    }
  }

  def setDotsData(data: Map[String, (Double, Double)]): Unit = setLinesData(data.map{
    case (name, pointData) => name -> (pointData :: Nil)
  })
}
