package it.unibo.pps.ese.view.configuration.dialogs.animaldialogs.genedialogs.custompropertiesdialog

import javafx.scene.Node

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.stage.Window

case class ConversionMapDialog(window: Window, currentConversion: Option[(String, Double)], qualites: Set[String]) extends Dialog[(String, Double)] {

  initOwner(window)
  title = "Conversion Map Dialog"
  headerText = "Define a conversion map"

  // Set the button types.
  val okButtonType = new ButtonType("Confirm", ButtonData.OKDone)
  dialogPane().buttonTypes = Seq(okButtonType)


  val conversionName = new ComboBox(ObservableBuffer[String](qualites.toSeq))
  val previousConversionName = new TextField()

  val conversionValue: TextField = new TextField() {
    promptText = "Value"
  }

  val requiredField = Seq(conversionName, conversionValue)

  val grid: GridPane = new GridPane() {
    hgap = 10
    vgap = 10
    padding = Insets(20, 100, 10, 10)

    add(new Label("Name"), 0, 0)
    add(if (currentConversion.isDefined) previousConversionName else conversionName, 1, 0)
    add(new Label("Value"), 0, 1)
    add(conversionValue, 1, 1)
  }


  // Enable/Disable login button depending on whether a username was
  // entered.
  val okButton: Node = dialogPane().lookupButton(okButtonType)
  okButton.disable = false


  dialogPane().content = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }

  if (currentConversion.isDefined) {
    conversionName.value.value = currentConversion.get._1
    previousConversionName.editable = false
    previousConversionName.text.value = currentConversion.get._1
    conversionValue.text.value = currentConversion.get._2.toString
  }

  resultConverter = dialogButton =>
    if (dialogButton == okButtonType) (conversionName.value.value, conversionValue.text.value.toDouble)
    else null
}


