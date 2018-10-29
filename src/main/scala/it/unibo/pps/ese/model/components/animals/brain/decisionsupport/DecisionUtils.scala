package it.unibo.pps.ese.model.components.animals.brain.decisionsupport

import it.unibo.pps.ese.controller.simulation.DynamicRules
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityKinds.EntityKinds
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.GenderTypes.GenderTypes

/**
  * Hierarchy of types, and related constraints, which describes the world in which decision support operates.
  */
trait WorldTypes {

  /**
    * Types that define the intrinsic properties of the entities.
    */
  type Name
  type Kind
  type Gender

  /**
    * Types that define the units of measurement in the world.
    */
  type HeightMeasure
  type AttackMeasure
  type PositionMeasure <: Int
  type AttractivenessMeasure

  /**
    * Compound type that describes the entity through both the intrinsic properties and the units of measurement of the world.
    */
  type EntityAttributes <: {
    def name: Name
    def kind: Kind
    def height: HeightMeasure
    def strength: AttackMeasure
    def defense: AttackMeasure
    def position: GeneralPosition[PositionMeasure]
    def attractiveness: AttractivenessMeasure
    def gender: Gender
  }

  /**
    * Compound utility type with which decision support makes explicit choices.
    */
  type EntityChoice <: {
    def name: Name
    def distance: PositionMeasure
  }

  /**
    * Compound type that describes the rules that govern the world.
    */
  type WorldRules <: {
    def attackThreshold: AttackMeasure
    def heightThresholds: HeightMeasure
    def compatibleHuntingKinds: Set[(Kind, Kind)]
    def compatibleCouplingKinds: Set[(Kind, Kind)]
  }
}

/**
  * Possible interpretation of the world where all types are defined respecting the relative constraints.
  */
trait WorldTypesImpl extends WorldTypes {
  type Name = String
  type Kind = EntityKinds
  type Gender = GenderTypes

  type HeightMeasure = Double
  type AttackMeasure = Double
  type PositionMeasure = Int
  type AttractivenessMeasure = Double

  type EntityAttributes = EntityAttributesImpl
  type EntityChoice = EntityChoiceImpl
  type WorldRules = WorldRulesImpl
}

/**
  * Enumeration that defines the possible genders of the entities.
  */
object GenderTypes extends Enumeration {
  type GenderTypes = Value
  val male, female = Value

  /**
    * Allows to know the element of the enum related to it in string format.
    * @param s element in string format
    * @return an option of the enum element
    */
  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}

/**
  * Enumeration that defines the possible species of the entities.
  */
object EntityKinds extends Enumeration {
  type EntityKinds = Value

  private var entityKinds: Set[String] = _
  private var constants: Map[Symbol, EntityKinds] = _
  updateSpecies()

  /**
    * Whenever other species appear in the world, through the singleton [[DynamicRules]], these are updated.
    */
  def updateSpecies(): Unit = {
    entityKinds = DynamicRules.instance().getSpecies
    entityKinds.foreach(Value)
    constants = entityKinds.map(v => Symbol(v) -> withName(v)).toMap
  }

  def apply(c: Symbol): EntityKinds = constants(c)
  def unapply(arg: EntityKinds): Option[Symbol] = Some(Symbol(values.find(x => arg.equals(x)).get.toString))
}

/**
  * Regardless of the space unit adopted, it allows to interpret the position of an entity with two coordinates.
  * @param x the abscissa coordinate
  * @param y the ordinate coordinate
  * @tparam PositionMeasure the type of space unit
  */
case class GeneralPosition[PositionMeasure <: Int](x: PositionMeasure, y: PositionMeasure) {
  /**
    * It allows to know the position of the point in question in relation to the point taken as a parameter, comparing the abscissas.
    * @param generalPosition the point to compare
    * @return 0 if they have the same abscissa, 1 if this has abscissa greater, -1 otherwise
    */
  def sameAbscissa(generalPosition: GeneralPosition[PositionMeasure]): Int = if (x == generalPosition.x) 0 else if (x > generalPosition.x) 1 else -1

  /**
    * It allows to know the position of the point in question in relation to the point taken as a parameter, comparing the ordinates.
    * @param generalPosition the point to compare
    * @return 0 if they have the same ordinate, 1 if this has ordinate greater, -1 otherwise
    */
  def sameOrdinate(generalPosition: GeneralPosition[PositionMeasure]): Int = if (y == generalPosition.y) 0 else if (y > generalPosition.y) 1 else -1
}


/**
  * A wrapper for animal attributes.
  */
object AnimalAttributes {
  def apply(name: String, kind: EntityKinds, height: Double, strength: Double, defense: Double, position: GeneralPosition[Int], attractiveness: Double, gender: GenderTypes): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, strength, defense, position, attractiveness, gender)
}

/**
  * A wrapper for plant attributes.
  */
object PlantAttributes {
  def apply(name: String, kind: EntityKinds, height: Double, defense: Double, position: GeneralPosition[Int], gender: GenderTypes): EntityAttributesImpl = EntityAttributesImpl(name, kind, height, 0, defense, position, 0, gender)
}

/**
  * Some utils that allow to write attributes easily.
  */
object EntityAttributesImpl {
  def apply(name: String, kind: EntityKinds, height: Double, strength: Double, defense: Double, position: GeneralPosition[Int], attractiveness: Double, gender: GenderTypes): EntityAttributesImpl =
    EntityAttributesImpl(name, kind, height, strength, defense, position, attractiveness, gender)

  implicit def generalPositionToTuple(generalPosition: GeneralPosition[Int]): (Int, Int) = (generalPosition.x, generalPosition.y)
  implicit def tupleToGeneralPosition(tuple: (Int, Int)): GeneralPosition[Int] = GeneralPosition(tuple._1, tuple._2)

  /**
    * A specific implementation that offers a interpretation of entities.
    * @param name the entity identifier
    * @param kind the entity species
    * @param height the entity height
    * @param strength the ability of the entity to attack
    * @param defense the ability of the entity to defense
    * @param position the position of the entity specified in coordinates
    * @param attractiveness the charm of the entity that involves the modalities of coupling
    * @param gender the entity gender
    */
  case class EntityAttributesImpl(name: String, kind: EntityKinds, height: Double, strength: Double, defense: Double, var position: GeneralPosition[Int], attractiveness: Double, gender: GenderTypes){
    override def toString: String = "Entity(" + name + ", " + kind + ", " + height + ", " + strength + ", " + defense + ", [" + position.x + ", " + position.y + "], " + attractiveness + ", " + gender + ")"
  }
}

/**
  * A specific implementation of choice that can be use by decision support.
  * @param name the target entity identifier
  * @param distance the distance between the source and the target
  */
case class EntityChoiceImpl(name: String, distance: Int)


/**
  * Listener who reacts whenever the rules of the world change.
  */
trait WorldRulesListener {
  /**
    * It allows notification of modification of hunting rules.
    * @param set the pair of species for hunting
    */
  def updateCouplingKind(set: Set[(String, String)]): Unit

  /**
    * It allows notification of modification of coupling rules.
    * @param set the pair of species for hunting
    */
  def updateHuntingKind(set: Set[(String, String)]): Unit
}

/**
  * A specific implementation that specify the rules that govern the world
  * @param attackThreshold the threshold that the difference between the attack of the hunter and the defense of the prey must overcome to complete the hunt
  * @param heightThresholds the threshold that the difference in heights between entities must exceed to bring the action they are about to perform
  * @param couplingThreshold the minimum threshold of attractiveness that entities must have to couple without resorting to violence
  * @param compatibleHuntingKinds all possible combinations "species of the hunter" "species of the prey"
  * @param compatibleCouplingKinds all combinations of species that can coupling together
  */
case class WorldRulesImpl(attackThreshold: Double,
                          heightThresholds: Double,
                          couplingThreshold: Double,
                          var compatibleHuntingKinds: Set[(EntityKinds, EntityKinds)] = Set.empty,
                          var compatibleCouplingKinds: Set[(EntityKinds, EntityKinds)] = Set.empty) {
  import WorldRulesImplUtils._

  private var listeners: Seq[WorldRulesListener] = Seq.empty

  /**
    * It allows the listeners to be register.
    * @param impl the listener reference
    */
  def addListener(impl: WorldRulesListener): Unit = listeners = listeners :+ impl

  /**
    * It allows the modification of hunting rules.
    * @param set the pair of species for hunting
    */
  def setCompatibleHuntingKinds(set: Set[(String, String)]): Unit = {
    compatibleHuntingKinds = set
    listeners.foreach(_.updateHuntingKind(set))
  }

  /**
    * It allows the modification of coupling rules.
    * @param set the pair of species for coupling
    */
  def setCompatibleCouplingKinds(set: Set[(String, String)]): Unit = {
    compatibleCouplingKinds = set
    listeners.foreach(_.updateCouplingKind(set))
  }
}

/**
  * Some utils that allow to write rules easily.
  */
object WorldRulesImplUtils {
  implicit def stringToEntityKinds(string: String): EntityKinds = EntityKinds(Symbol(string))
  implicit def tupleStringToEntityKinds(tuple: (String, String)): (EntityKinds, EntityKinds) = (tuple._1, tuple._2)
  implicit def setTupleStringToSetTupleEntityKinds(set: Set[(String, String)]): Set[(EntityKinds, EntityKinds)] = set map tupleStringToEntityKinds
}



