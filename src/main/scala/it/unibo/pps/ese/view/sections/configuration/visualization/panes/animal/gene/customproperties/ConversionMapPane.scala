package it.unibo.pps.ese.view.sections.configuration.visualization.panes.animal.gene.customproperties

import it.unibo.pps.ese.view.sections.configuration.visualization.core.{AbstractPane, MainDialog, Modality}
import it.unibo.pps.ese.view.sections.configuration.visualization.core.components.{ErrorLabel, WhiteLabel}

import scala.collection.immutable.ListMap
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, VBox}

/**
  * It defines the title and the header
  */
object ConversionMapProperties {
    val title = "Conversion Map Pane"
    val headerText = "Define a conversion map"
}

import ConversionMapProperties._
import it.unibo.pps.ese.view.sections.configuration.visualization.core.PaneProperties._

/**
  * The pane that allows to specify the conversion map between a property and a quality.
  *
  * @param mainDialog the main dialog with which communicating
  * @param previousContent the previous content
  * @param modality add or modify
  * @param currentConversion the previous conversion map if the modality is modify
  * @param qualities the qualities that can be set
  */
case class ConversionMapPane(mainDialog: MainDialog,
                             override val previousContent: Option[PropertiesPane],
                             modality: Modality,
                             currentConversion: Option[(String, Double)],
                             qualities: Set[String])
  extends AbstractPane[(String, Double)](mainDialog, previousContent, None, title, headerText, previousContent.get.path + newLine(5) + title, 5) {

  /*
  Fields
   */

  val conversionName = new ComboBox(ObservableBuffer[String](qualities.toSeq))
  val previousConversionName = new TextField()

  val conversionValue: TextField = new TextField()

  fields = ListMap(
    conversionValue -> (new WhiteLabel("Value"), new ErrorLabel("")),
  )

  val grid: GridPane = createGrid(1)
  grid.vgap = 10
  grid.add(new WhiteLabel("Name"), 0, 0)
  grid.add(if (currentConversion.isDefined) previousConversionName else conversionName, 1, 0)

  center = new VBox() {
    children ++= Seq(grid)
    styleClass += "sample-page"
  }


  /*
  Checks
   */

  mandatoryFields = fields.keySet
  doubleFields = fields.keySet

  createChecks()

  /*
  Restart information
  */

  if (currentConversion.isDefined) {
    conversionName.value.value = currentConversion.get._1
    previousConversionName.editable = false
    previousConversionName.text.value = currentConversion.get._1
    conversionValue.text.value = currentConversion.get._2.toString
    okButton.disable = false
  } else {
    conversionName.value.value = qualities.head
  }

  /*
  Result
  */

  okButton.onAction = _ => {
    previousContent.get.confirmConversionMap(modality, conversionName.value.value, conversionValue.text.value.toDouble)
  }

}



