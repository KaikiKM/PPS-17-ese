package it.unibo.pps.ese.model.components.animals.brain.decisionsupport

import it.unibo.pps.ese.controller.simulation.DynamicRules
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.WorldRulesImplUtils._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.prologimplementation.PrologDecisionSupport

/**
  * Enumeration that catalog the decision support implementation.
  */
trait DecisionSupportImplementations
case object PrologImplementation extends DecisionSupportImplementations
case object ScalaImplementation extends DecisionSupportImplementations

/**
  * App that calculates the performance of each decision support implementation.
  */
object PerformanceTest extends App {

  /*
  Definition of the world rules.
   */
  DynamicRules.instance().addSpecies(Set("carnivorous", "herbivore", "plant"))
  val worldRules: WorldRulesImpl = WorldRulesImpl(3,  5, 3, Set(("carnivorous", "herbivore"), ("herbivore", "plant")),
    Set(("carnivorous", "carnivorous"), ("herbivore", "herbivore")))

  DynamicRules.instance().setRules(worldRules)


  /*
  Definition of the entity attributes.
   */
  val prey0 = EntityAttributesImpl("a", EntityKinds('plant), 5, 2, 2, (5, 6), 5, GenderTypes.male)
  val prey1 = EntityAttributesImpl("b", EntityKinds('herbivore), 8, 6, 6, (6, 6), 5, GenderTypes.male)
  val prey2 = EntityAttributesImpl("c", EntityKinds('herbivore), 7, 7, 7, (2, 1), 5, GenderTypes.female)
  val prey3 = EntityAttributesImpl("d", EntityKinds('herbivore), 6, 6, 6, (3, 1), 5, GenderTypes.male)
  val prey4 = EntityAttributesImpl("e", EntityKinds('carnivorous), 10, 10, 10, (3, 3), 5, GenderTypes.male)
  val prey5 = EntityAttributesImpl("f", EntityKinds('carnivorous), 9, 9, 9, (4, 3), 5, GenderTypes.male)


  var decisionSupport: DecisionSupport = _

  /*
  Iterations for the performance test.
   */
  List(ScalaImplementation, PrologImplementation).foreach(impl => {
    decisionSupport = implement(impl)

    val startTime = System.nanoTime

    for(_ <- 1 to 100) {
      decisionSupport.createVisualField(Seq(prey1, prey2, prey3, prey4, prey5))
      decisionSupport.discoverPartners(prey1) foreach (println(_))
      decisionSupport.clearVisualField()
    }

    val estimatedTime = System.nanoTime - startTime
    println(impl + " " + estimatedTime)
  })

  /**
    * It allows to switch the decision support implementation.
    * @param implementation the enumeration that allows to catalog the decision support implementation
    * @return the decision support instance
    */
  private def implement(implementation: DecisionSupportImplementations): DecisionSupport = implementation match {
    case PrologImplementation => PrologDecisionSupport()
    case ScalaImplementation => DecisionSupport()
  }

}
