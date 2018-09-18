package it.unibo.pps.ese.view.start

import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ScrollPane}
import scalafx.scene.text.{Text, TextFlow}
import scalafx.stage.Window
import scalafx.Includes._

trait UnexpectedExceptionAlert extends Alert

object UnexpectedExceptionAlert {

  def apply(owner: Window, exception: Throwable): UnexpectedExceptionAlert = new UnexpectedExceptionAlertImpl(owner, exception)

  private class UnexpectedExceptionAlertImpl(owner: Window, exception: Throwable) extends Alert(AlertType.Error) with UnexpectedExceptionAlert {
    initOwner(owner)
    title = "ERROR!"
    headerText = "An unexpected exception occurred!"
    dialogPane().expandableContent = new ScrollPane() {
      content = new TextFlow() {
        children.add(new Text(exception.toString))
      }
    }
  }
}