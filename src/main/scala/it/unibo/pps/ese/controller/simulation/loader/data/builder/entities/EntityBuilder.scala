package it.unibo.pps.ese.controller.simulation.loader.data.builder.entities

import it.unibo.pps.ese.controller.simulation.loader.data.builder.BuilderStatus
import it.unibo.pps.ese.controller.simulation.loader.data.builder.entities.EntityStatus.{EntityWithAlleleLength, EntityWithGeneLength, EntityWithReign, PlantWithHardness, PlantWithNutritionalValue, _}

import scala.reflect.runtime.universe._

trait EntityBuilder[S <: EntityStatus] { self =>
  type RET[A <: EntityStatus]
  def setName(name: String): RET[S with EntityWithName]
  def setGeneLength(geneLength: Int): RET[S with EntityWithGeneLength]
  def setAlleleLength(alleleLength: Int): RET[S with EntityWithAlleleLength]
  def setReign(reign: String): RET[S with EntityWithReign]
  def status: TypeTag[S]
}

abstract class EntityBuilderImpl[S <: EntityStatus](name: Option[String],
                                                   geneLength: Option[Int],
                                                   alleleLength: Option[Int],
                                                   reign: Option[String])(implicit private val test: TypeTag[S]) extends EntityBuilder[S]{

  def setName(name: String): RET[S with EntityWithName] =
    newInstance(Some(name), geneLength, alleleLength, reign)

  def setGeneLength(geneLength: Int): RET[S with EntityWithGeneLength] =
    newInstance(name, Some(geneLength), alleleLength, reign)

  def setAlleleLength(alleleLength: Int): RET[S with EntityWithAlleleLength] =
    newInstance(name, geneLength, Some(alleleLength), reign)

  def setReign(reign: String): RET[S with EntityWithReign] =
    newInstance(name, geneLength, alleleLength, Some(reign))

  def newInstance[NT <: EntityStatus](name: Option[String], geneLength: Option[Int], alleleLength: Option[Int],
                                      reign: Option[String])(implicit tt: TypeTag[NT]): RET[NT]
}

sealed trait EntityStatus extends BuilderStatus
object EntityStatus {
  sealed trait EmptyEntity extends EntityStatus
  sealed trait EntityWithName extends EntityStatus
  sealed trait EntityWithGeneLength extends EntityStatus
  sealed trait EntityWithAlleleLength extends EntityStatus
  sealed trait EntityWithReign extends EntityStatus
  sealed trait PlantWithNutritionalValue extends EntityStatus
  sealed trait PlantWithHardness extends EntityStatus
  sealed trait PlantWithHeight extends EntityStatus
  sealed trait AnimalWithTypology extends EntityStatus
  sealed trait AnimalWithStructChromosome extends EntityStatus
  sealed trait AnimalWithRegChromosome extends EntityStatus
  sealed trait AnimalWithSexChromosome extends EntityStatus

  type ValidEntity = EmptyEntity with EntityWithName
  type FullEntity = ValidEntity with  EntityWithGeneLength with EntityWithAlleleLength with EntityWithReign
  type FullPlant = FullEntity  with PlantWithHeight with PlantWithNutritionalValue with PlantWithHardness
  type FullAnimal = FullEntity with AnimalWithTypology with AnimalWithStructChromosome with AnimalWithRegChromosome
    with AnimalWithSexChromosome
}