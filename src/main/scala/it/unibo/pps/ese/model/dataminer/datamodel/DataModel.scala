package it.unibo.pps.ese.model.dataminer.datamodel

import it.unibo.pps.ese.model.components.animals.brain.ActionTypes
import it.unibo.pps.ese.model.dataminer.DataModelSupport._
import it.unibo.pps.ese.utils.Point


/**
  * These traits define how the simulation data are historicised.
  * Data is aggregated on entities rather than eras. So the main entity in model are EntityLogs, containing
  * all the info on a Simulation's entity. The data is categorized in:
  * Structural: containing static info
  * Dynamic: containing dynamic info. These info are structured in collections of samples of the Entities' dynamic
  *          properties associated to the sampling era
  */

trait EntityLog {
  val id: EntityId
  val structuralData: StructuralData
  val dynamicData: Seq[(Era, DynamicData)]
}

trait EntityTimedRecord {
  val id: EntityId
  val era: Era
  val structuralData: StructuralData
  val dynamicData: DynamicData
}

trait EntityStaticRecord {
  val id: EntityId
  val data: StructuralData
}

trait EntityDynamicRecord {
  val id: EntityId
  val data: DynamicData
}

trait DynamicData {
  val position: Point
  val nutritionalValue: Double
}

trait AnimalDynamicData extends DynamicData {
  val age: Integer
  val energy: Double
  val lifePhase: LifePhase
  val speed: Double
  val fertility: Double
  val coupling: Seq[String]
  val eating: Seq[String]
  val givingBirth: Seq[String]
  val producedMutantGenes: Seq[String]
  val will: ActionTypes
}

trait PlantDynamicData extends DynamicData

trait StructuralData extends {
  val species: Species
  val reign: Reign
  val gender: Sex
  val diet: Diet
  val height: Double
  val defense: Double
}

trait AnimalStructuralData extends StructuralData {
  val strength: Double
  val actionField: Double
  val visualField: Double
  val averageLife: Double
  val energyRequirements: Double
  val maturity: Double
  val oldness: Double
  val decay: Double
  val speed: Double
  val fertility: Double
}

trait PlantStructuralData extends StructuralData