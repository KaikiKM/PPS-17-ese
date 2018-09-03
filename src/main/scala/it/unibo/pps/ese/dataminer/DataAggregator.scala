package it.unibo.pps.ese.dataminer

import it.unibo.pps.ese.genericworld.model.{EntityInfo, EntityState, ReignType}
import it.unibo.pps.ese.genericworld.model.EntityInfoConversion._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

object DataAggregator {

  private val _entityDataRepository = EntityDataRepository()

  private def mapToAnimalStructuralData(state: EntityInfo): StructuralData =
    AnimalStructuralDataImpl(
      state strong,
      state actionField,
      state visualField,
      state averageLife,
      state energyRequirements,
      state endChildPhase,
      state endAdultPhase,
      state percentageDecay,
      state speed,
      state fertility,
      state.species.toString,
      state.reign.toString,
      state.gender.toString,
      state.diet.toString,
      state height,
      state defense
    )

  private def mapToPlantStructuralData(state: EntityInfo): StructuralData =
    PlantStructuralDataImpl(
      state availability,
      state.species.toString,
      state.reign.toString,
      state.gender.toString,
      state.diet.toString,
      state height,
      state defense
    )

  private def mapToAnimalDynamicData(state: EntityInfo): DynamicData =
    AnimalDynamicDataImpl(
      state age,
      state energy,
      state.lifePhase.toString,
      state actualSpeed,
      state position,
      state nutritionalValue
    )

  private def mapToPlantDynamicData(state: EntityInfo): DynamicData =
    PlantDynamicDataImpl(
      state position,
      state nutritionalValue
    )

  private def mapToStructuralData(state: EntityInfo): StructuralData = {
    if (state.reign == ReignType.ANIMAL) mapToAnimalStructuralData(state)
    else mapToPlantStructuralData(state)
  }

  private def mapToDynamicData(state: EntityInfo): DynamicData = {
    if (state.reign == ReignType.ANIMAL) mapToAnimalDynamicData(state)
    else mapToPlantDynamicData(state)
  }

  private implicit def mapToStaticRecord(state: EntityState): EntityStaticRecord = {
    EntityStaticRecordImpl(state entityId, mapToStructuralData(state.state))
  }

  private implicit def mapToDynamicRecord(state: EntityState): EntityDynamicRecord = {
    EntityDynamicRecordImpl(state entityId, mapToDynamicData(state state))
  }

  @tailrec
  def ingestData(era: Era, data: Seq[EntityState])(implicit context: ExecutionContext): Unit = {
    if (data isEmpty) return
    if (!(_entityDataRepository exists ((data head) entityId))) _entityDataRepository saveStaticEntityData (data head)
    _entityDataRepository saveDynamicEntityData (era, data head)
    ingestData(era, data tail)
  }

  def ingestedData: ReadOnlyEntityRepository = _entityDataRepository
}