package it.unibo.pps.ese.model.components.animals.brain.decisionsupport

import it.unibo.pps.ese.controller.simulation.StaticRules
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImplUtils._
import org.scalatest.FunSuite
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImplUtils._


class DecisionSupportTest extends FunSuite {

  /*
  Definition of the world rules.
  */
  StaticRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = WorldRulesImpl(3, 5, 3, Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))
  StaticRules.instance().setRules(worldRules)

  /*
  Definition of the entity attributes.
   */
  val prey1 = EntityAttributesImpl("1", EntityKinds('herbivore), 6, 6, 6, (6, 6), 5, GenderTypes.male)
  val prey2 = EntityAttributesImpl("2", EntityKinds('herbivore), 6, 6, 6, (2, 1), 5, GenderTypes.female)
  val hunter = EntityAttributesImpl("3", EntityKinds('carnivorous), 10, 10, 10, (3, 3), 5, GenderTypes.male)

  /*
  Tests.
   */
  val decisionSupport: DecisionSupport = DecisionSupport()
  decisionSupport.createVisualField(Seq(prey2, hunter))
  val firstTest: Stream[EntityChoiceImpl] = decisionSupport.discoverPreys(hunter)

  test("Hunter with a prey has a not empty result.") {
    assert(firstTest.lengthCompare(1) == 0)
  }

  val newHunterPosition: GeneralPosition[Int] = decisionSupport.nextMove(hunter, prey2)

  test("The new calculated position must be closer than the original.") {
    val originalDistance = (hunter.position._1 - prey2.position._1) + (hunter.position._2 - prey2.position._2)
    val newDistance = (newHunterPosition.x - prey2.position._1) + (newHunterPosition.y - prey2.position._2)
    assert(newDistance < originalDistance)
  }

  val decisionSupport2: DecisionSupport = DecisionSupport()
  decisionSupport2.createVisualField(Seq(prey1, prey2, hunter))
  decisionSupport2.clearVisualField()
  val thirdTest: Stream[EntityChoiceImpl] = decisionSupport2.discoverPreys(hunter)

  test("clearVisualField must delete all entities.") {
    assert(thirdTest.lengthCompare(0) == 0)
  }
}
