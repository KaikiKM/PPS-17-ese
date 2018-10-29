package it.unibo.pps.ese.model.components.animals.brain.decisionsupport


import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.controller.simulation.DynamicRules

import scala.math._

/**
  * Trait that allows a subject to enter entities in his field of vision, classify them and calculate the best path to each.
  * More specifically a series of cascaded filters are applied to each entity for each class it can take.
  * An entity is cataloged with that specific class only if it exceeds all the filters to which it has been subjected.
  */
trait DecisionSupport extends WorldTypesImpl {

  /**
    * Rules of the world, acquired through the singleton [[DynamicRules]], needed to filter each entity in the visual field.
    */
  val worldRules: WorldRules = DynamicRules.instance().getRules

  implicit def tupleToEntityChoice(tuple: (Name, Int)): EntityChoice = new EntityChoice(tuple._1, tuple._2)
  implicit def streamTupleToStreamEntityChoice(tuples: Stream[(Name, Int)]): Stream[EntityChoice] = tuples map tupleToEntityChoice

  /**
    * It allows to enter the entities in the visual field.
    * @param entitiesAttributes the attributes of the entities in the visual field
    */
  def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit

  /**
    * It allows to clean the visual field to start the next iteration.
    */
  def clearVisualField(): Unit

  /**
    * The method that allows the hunter to know the preys in his visual field.
    * @param hunter the attributes of the entity that want to hunt
    * @return a stream of entities and relative distance between the hunter and them
    */
  def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice]

  /**
    * The method that allows an entity to know the possible partners in his visual field.
    * @param entity the attributes of the entity that want to meet partners
    * @return a stream of entities and relative distance between the entity and them
    */
  def discoverPartners(entity: EntityAttributes): Stream[EntityChoice]

  /**
    * It allows to know the next move to take to reach the target entity.
    * @param from the attributes of the entity in question
    * @param to the attributes of the entity target
    * @return the position to take
    */
  def nextMove(from: EntityAttributes, to: EntityAttributes): GeneralPosition[PositionMeasure]
}


/**
  * A specific Scala Implementation of the trait [[DecisionSupport]].
  */
object DecisionSupport {
  def apply(): DecisionSupport = new DecisionSupportImpl()

  private class DecisionSupportImpl() extends DecisionSupport {

    /**
      * The field that storage the current visual field.
      */
    private var visualField: Set[EntityAttributes] = Set.empty

    override def createVisualField(entitiesAttributes: Seq[EntityAttributes]): Unit = visualField ++= entitiesAttributes

    override def clearVisualField(): Unit = visualField = Set.empty

    override def discoverPreys(hunter: EntityAttributes): Stream[EntityChoice] =
      basicFilter(hunter) filter (prey => compatiblePreysKind(hunter, prey)) filter (prey => simulateAttack(hunter, prey)) map (prey => calculateLength(hunter, prey))

    override def discoverPartners(hunter: EntityAttributes): Stream[EntityChoice] =
      basicFilter(hunter) filter (partner => compatiblePartnersKind(hunter, partner)) filter (partner => simulateCoupling(hunter, partner)) map (partner => calculateLength(hunter, partner))

    override def nextMove(from: EntityAttributes, to: EntityAttributes): GeneralPosition[PositionMeasure] = (from.position, to.position) match  {
      case (f, t) if (f sameAbscissa t) > 0 => (f.x - 1, f.y)
      case (f, t) if (f sameAbscissa t) < 0 => (f.x + 1, f.y)
      case (f, t) if (f sameOrdinate t) > 0 => (f.x, f.y - 1)
      case (f, t) if (f sameOrdinate t) < 0 => (f.x, f.y + 1)
      case (f, _) => f
    }

    /**
      * Regardless of whether you want to find out if the entities can be a preys or a partners, you need to apply filters on the height and exclude the hunter/partner
      * @param entity the attributes of the entity that want to meet partners or hunt preys
      * @return the visual field filtered
      */
    private def basicFilter(entity: EntityAttributes): Stream[EntityAttributes] = {
      visualField filter (prey => prey != entity) filter (other => heightFilter(entity, other)) toStream
    }

    /**
      * It applies the height filter
      * @param entity1 first entity
      * @param entity2 second entity
      * @return if the height is adequate
      */
    private def heightFilter(entity1: EntityAttributes, entity2: EntityAttributes): Boolean = {
      val diff = abs(entity1.height - entity2.height)
      diff < worldRules.heightThresholds
    }

    /**
      * It checks if the species of the entities are suitable for the hunt
      * @param hunter the hunter attributes
      * @param prey the prey attributes
      * @return if the species are suitable
      */
    private def compatiblePreysKind(hunter: EntityAttributes, prey: EntityAttributes): Boolean =
      worldRules.compatibleHuntingKinds.contains((hunter.kind, prey.kind))

    /**
      * It checks if the species of the entities are suitable for the coupling
      * @param entity the attributes of the entity that want to meet
      * @param partner the partner attributes
      * @return if the species are suitable
      */
    private def compatiblePartnersKind(entity: EntityAttributes, partner: EntityAttributes): Boolean =
      worldRules.compatibleCouplingKinds.contains((entity.kind, partner.kind))

    /**
      * It simulates an attack.
      * @param hunter the hunter attributes
      * @param prey the prey attributes
      * @return if the strength of the hunter is greater the defense of the prey, of the attack threshold.
      */
    private def simulateAttack(hunter: EntityAttributes, prey: EntityAttributes): Boolean =
      (hunter.strength - prey.defense) > worldRules.attackThreshold

    /**
      * It simulates a flirt.
      * Only male entities can decide to flirt.
      * These do it only if the female is quite attractive.
      * If even the male is attractive, then he tries otherwise he simulates an attack as if he were to force the coupling.
      * @param entity the attributes of the entity that want to meet
      * @param partner the partner attributes
      * @return if the coupling went well
      */
    private def simulateCoupling(entity: EntityAttributes, partner: EntityAttributes): Boolean = entity.gender == GenderTypes.male && partner.gender == GenderTypes.female &&
      partner.attractiveness > worldRules.couplingThreshold && (simulateAttack(entity, partner) || entity.attractiveness > worldRules.couplingThreshold)

    /**
      * Calculate the length of the path between the hunter/partner and the prey/other partner.
      * @param entity1 the first entity
      * @param entity2 the second entity
      * @return the path length
      */
    private def calculateLength(entity1: EntityAttributes, entity2: EntityAttributes): EntityChoice =
      (entity2.name, abs(entity1.position.x - entity2.position.x) + abs(entity1.position.y - entity2.position.y))

  }

}

