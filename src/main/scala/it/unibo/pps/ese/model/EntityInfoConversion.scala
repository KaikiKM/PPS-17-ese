package it.unibo.pps.ese.model

import scala.language.implicitConversions

object EntityInfoConversion {
  implicit class ExampleComponentConversions(obj: EntityInfo) {
    def speed : Int = obj.selectDynamic("speed").asInstanceOf[Int]
  }
}
