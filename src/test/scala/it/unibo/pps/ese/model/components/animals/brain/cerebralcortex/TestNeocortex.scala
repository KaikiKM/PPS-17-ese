package it.unibo.pps.ese.model.components.animals.brain.cerebralcortex

import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.Memory.{LongTermMemory, ShortTermMemory}
import it.unibo.pps.ese.utils.Position
import org.scalatest.FunSuite

class TestNeocortex extends FunSuite {

  private object Properties {
    val worldWidth = 1000
    val worldHeight = 1000
    val locationalFieldSize = 5
  }

  test("Test neocortex") {
    import Properties._

    val neocortex: Neocortex = Neocortex()
    neocortex.addMemory(Hunting, LongTermMemory(Hunting, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(0,20)), 150))
    neocortex.addMemory(Hunting, LongTermMemory(Hunting, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(10,30)), 160))
    neocortex.addMemory(Couple, LongTermMemory(Hunting, LocationalField(worldWidth, worldHeight, locationalFieldSize, Position(100,130)), 260))
    assert(neocortex.getMemeories(Hunting).get.size==2)
    assert(neocortex.getMemeories(Couple).get.size==1)
  }
}
