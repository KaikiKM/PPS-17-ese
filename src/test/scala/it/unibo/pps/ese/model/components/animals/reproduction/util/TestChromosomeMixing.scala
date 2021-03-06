package it.unibo.pps.ese.model.components.animals.reproduction.util

import it.unibo.pps.ese.controller.simulation.loader.YamlLoader
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.CompleteSimulationData
import it.unibo.pps.ese.controller.simulation.loader.io.File
import it.unibo.pps.ese.model.genetics.GeneticsSimulator
import it.unibo.pps.ese.model.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.model.genetics.dna.{AnimalGenome, BasicGene, Chromosome, ChromosomeCouple, ChromosomeType, MGene, RegulatorGene, SexualChromosomeCouple, StructuralGene, X}
import it.unibo.pps.ese.model.genetics.entities.AnimalInfo
import org.kaikikm.threadresloader.ResourceLoader
import org.scalatest.FunSuite

class TestChromosomeMixing extends FunSuite {

  test("Chromosomes couples can be correctly mixed without mutations") {
    implicit val fake: GeneticsEngine = new GeneticsEngine {
      override def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo = null

      override def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene] = Seq()

      override val mutationProb: Double = 0
    }

    val bg1 = BasicGene(Seq('A'), StructuralGene)
    val bg2 = BasicGene(Seq('B'), StructuralGene)
    val bg3 = BasicGene(Seq('C'), StructuralGene)
    val bg4 = BasicGene(Seq('D'), StructuralGene)
    val c1 = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL, bg1, bg2)
    val c2 = Chromosome(ChromosomeType.STRUCTURAL_ANIMAL, bg3, bg4)

    val couple1 = ChromosomeCouple(c1, c1)
    val couple2 = ChromosomeCouple(c2, c2)

    val sonCouple = EmbryosUtil.generateNewChromosomeCouple(couple1, couple2, "test")
    require(sonCouple._1 == ChromosomeType.STRUCTURAL_ANIMAL)
    assert(sonCouple._2.firstChromosome == c1)
    assert(sonCouple._2.secondChromosome == c2)

    val sg1 = BasicGene(Seq('A'), RegulatorGene)
    val sg3 = BasicGene(Seq('C'), RegulatorGene)
    val sg4 = BasicGene(Seq('D'), RegulatorGene)
    val sc1 = Chromosome(ChromosomeType.SEXUAL_X, X, sg1, sg3, sg4)

    val sexualCouple1 = SexualChromosomeCouple(sc1, sc1)
    val sexualCouple2 = SexualChromosomeCouple(sc1, sc1)

    val sonSexCouple = EmbryosUtil.generateNewChromosomeCouple(sexualCouple1, sexualCouple2, "test")
    assert(sonSexCouple.firstChromosome == sc1)
  }

  test("Chromosomes couples can be correctly mixed with mutations") {
    val data = YamlLoader.loadSimulation(File(ResourceLoader.getResource("it/unibo/pps/ese/controller/simulation/loader/Simulation.yml"))).asInstanceOf[CompleteSimulationData]
    val initializedSimulation = GeneticsSimulator.beginSimulation(data)
    val animalGenome = initializedSimulation.getAllAnimals.head._2.head.genome

    implicit val validGeneticEngine: GeneticsEngine = new GeneticsEngine {
      override def getAnimalInfoByGenome(species: String, childGenome: AnimalGenome): AnimalInfo =
        GeneticsSimulator.getAnimalInfoByGenome(species, childGenome)

      override def obtainMutantAlleles(species: String, gene: MGene): Iterable[MGene] =
        GeneticsSimulator.obtainMutantAlleles(species, gene)

      override val mutationProb: Double = 1.0
    }

    val species = initializedSimulation.getAllAnimals.head._1
    val structuralChromosomeCouple = animalGenome.autosomeChromosomeCouples(ChromosomeType.STRUCTURAL_ANIMAL)
    val newChromosomeCouple = EmbryosUtil.generateNewChromosomeCouple(structuralChromosomeCouple,
      structuralChromosomeCouple, species)._2
    val expectedChromosomeCode: Seq[ProteinoGenicAmminoacid] = List('O', 'C', 'C', 'S', 'D', 'C')
    assert(newChromosomeCouple.firstChromosome.geneList.exists(gene => gene.completeCode == expectedChromosomeCode))
  }
}
