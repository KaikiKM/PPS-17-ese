package it.unibo.pps.ese.view.utilities

import javafx.scene.text.{Font, Text}
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.text.{TextAlignment, TextFlow}

/**
  * Some utilities to work with text
  */
object TextUtilities {

  /**
    * Pimp a string to transform it in various form
    * @param string
    */
  implicit class RichText(string:String){
    def toTextStyled(style:String,color: Color,font: Font):Text ={
      val text = new Text(string)
      text.setStyle(style)
      text.setFill(color)
      text.setFont(font)
      text
    }
    def toLabel:Label = {
      new Label(string)
    }
    def toLabelText:Text = {
      toTextStyled(
        style="-fx-font-weight: 900",
        color=Color.web("67809F"),
        font=Font.font("Calibri", 24)
      )
    }
    def toText:Text = {
      toTextStyled(
        style = "",
        color = Color.White,
        font = Font.font("Calibri", 24)
      )
    }
    def toTextOrgan:Text = {
      toTextStyled(
        style = "",
        color=Color.web("67809F"),
        font = Font.font("Calibri", 24)
      )
    }
    def toHBox:HBox = {
      val hBox = new HBox()
      val text:Text = toTextStyled(
        style = "",
        color = Color.Black,
        font = Font.font("Calibri", 24)
      )
      hBox.alignment = Pos.BaselineCenter
      text.textAlignment = TextAlignment.Center
      hBox.children += text
      hBox
    }
  }
  implicit class RichTextFlow(textFlow:TextFlow){
    import scala.collection.JavaConverters._
    def addAndClear(node:Text): Unit ={
      textFlow.children clear()
      textFlow.children add node
    }

    def addAllAndClear(nodes:Seq[Text]): Unit ={
      textFlow.children.clear()
      textFlow.children.addAll(nodes.asJava)
    }
  }
}
