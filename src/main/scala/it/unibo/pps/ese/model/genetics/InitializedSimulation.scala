package it.unibo.pps.ese.model.genetics

import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.model.genetics.dnaexpression.AlleleInfo
import it.unibo.pps.ese.model.genetics.entities.{AnimalInfo, PlantInfo}
import it.unibo.pps.ese.model.genetics.generators.data.InputDataAdapter
import it.unibo.pps.ese.model.genetics.generators.{PlantGenerator, SpeciesUtilities}

/**
  * An initialized simulation with the number of animals and plants set in the configuration
  */
sealed trait InitializedSimulation{
  private [genetics] def initialSetup:Map[String,SpeciesUtilities]

  /**
    * To obtain a Map with the name of the species as a key and a Seq of [[AnimalInfo]] as value
    * @return
    *         A Map with the name of the species as a key and a Seq of [[AnimalInfo]] as value
    */
  def getAllAnimals:Map[String,Seq[AnimalInfo]]
  /**
    * To obtain a Map with the name of the species as a key and a Seq of [[PlantInfo]] as value
    * @return
    *         A Map with the name of the species as a key and a Seq of [[PlantInfo]] as value
    */
  def getAllPlant:Map[String,Seq[PlantInfo]]
}

object InitializedSimulation {
  def apply(simulationData: CompleteSimulationData): InitializedSimulation = InitializedSimulationImpl(simulationData)
  def apply(simulationData: CompleteSimulationData,notAppearedMutations:Map[String,Seq[AlleleInfo]]): InitializedSimulation = {
    val initializedSimulation = InitializedSimulationImpl(simulationData)
    notAppearedMutations.foreach{case(k,v) =>
        initializedSimulation.initialSetup(k).restoreOldNotAppearedAlleles(v)
    }
    initializedSimulation
  }

  private[this] case class InitializedSimulationImpl(simulationData: CompleteSimulationData) extends InitializedSimulation{
    val speciesSetup:Map[String,SpeciesUtilities] = buildSpeciesSetups(simulationData)

    override private[genetics] def initialSetup = speciesSetup

    override val getAllAnimals: Map[String, Seq[AnimalInfo]] = {
      simulationData.animals.map(e=>(e._1.name,e._2)).map(c=>{
        val seq:Seq[AnimalInfo] = speciesSetup(c._1).generateNumberOfAnimal(c._2)
        (c._1,seq)
      })
    }

    override val getAllPlant: Map[String, Seq[PlantInfo]] = {
      simulationData.plants.map(c=>{
        val seq:Seq[PlantInfo] = ( 1 to c._2 by 1).map(i=> {
          PlantGenerator.createPlantInfoByPlantData(c._1)
        })
        (c._1.name,seq)
      })
    }
  }

  private[this] def buildSpeciesSetups(simulationData: CompleteSimulationData):Map[String,SpeciesUtilities] = {
    simulationData
      .animals
      .keys
      .map(e=>
        (e.name,
          SpeciesUtilities(InputDataAdapter.translateAnimalData(e))
        ))
      .toMap
  }
}
