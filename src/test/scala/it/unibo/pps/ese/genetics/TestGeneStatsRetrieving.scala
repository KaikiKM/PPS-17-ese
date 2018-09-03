package it.unibo.pps.ese.genetics

import it.unibo.pps.ese.controller.loader.YamlLoader
import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.genetics.dna.{ChromosomeCouple, ChromosomeType, GeneInChromosome, MGene}
import it.unibo.pps.ese.genetics.dnaexpression.{AllelicGeneStats, BasicGeneStats, GeneStats}
import it.unibo.pps.ese.genetics.entities.{Animal, AnimalInfo, Carnivorous, Female, Herbivore, Male, Plant, QualityType}
import org.scalatest.{Outcome, fixture}

class TestGeneStatsRetrieving extends fixture.FunSuite {

  type FixtureParam = GeneticsSimulator

  def withFixture(test: OneArgTest):Outcome={
    val data = new YamlLoader().loadSimulation("it/unibo/pps/ese/controller/loader/Simulation.yml")
    val geneticsSimulator:GeneticsSimulator = GeneticsSimulator
    geneticsSimulator.beginSimulation(data)
    test(geneticsSimulator)
  }

  test("Test retrieving BasicGeneStats"){ geneticsSimulator =>
    val animalInfo:AnimalInfo = geneticsSimulator.newAnimal("Gatto")
    val commonChromosome1 = animalInfo.genome.autosomeChromosomeCouples(ChromosomeType.COMMON).firstChromosome
    val commoneGeneStats:Seq[GeneStats] = geneStatsByChromosome(commonChromosome1,geneticsSimulator,animalInfo)
    val identifiedThings:Seq[String] = List(  "Species",
                                              Herbivore.toString,
                                              Carnivorous.toString,
                                              Animal.toString,
                                              Plant.toString)
    commoneGeneStats.foreach {
      case BasicGeneStats(g, id) => assert(identifiedThings.contains(id))
      case _ => fail()
    }

    val feedingChromosome1 = animalInfo.genome.autosomeChromosomeCouples(ChromosomeType.FEEDING).firstChromosome
    geneStatsByChromosome(feedingChromosome1,geneticsSimulator,animalInfo)
      .foreach {
        case BasicGeneStats(g,id) => assert(id == Carnivorous.toString)
        case _=> fail()
      }
  }


  test("Test retrieving AllelicGeneStats"){ geneticsSimulator=>
    val animalInfo:AnimalInfo = geneticsSimulator.newAnimal("Gatto")
    val gene1Seq:Seq[ProteinoGenicAmminoacid] = List('A','B','B','S','D','A')
    val gene2Seq:Seq[ProteinoGenicAmminoacid] = List('O','C','C','S','D','A')

    val structuralChromosome1 = animalInfo.genome.autosomeChromosomeCouples(ChromosomeType.STRUCTURAL_ANIMAL).firstChromosome
    geneStatsByChromosome(structuralChromosome1,geneticsSimulator,animalInfo)
      .foreach{
        case AllelicGeneStats(g,d,p,a,aq,f) if checkGeneEquality(g,gene1Seq)  =>
          assert(d == 5.0)
          assert(p == 1.0)
          assert(aq.toSet == Set(
            QualityType.Speed,
            QualityType.Height,
            QualityType.ResistenceToAttack,
            QualityType.RangeOfAction,
            QualityType.NutritionalValue)
          )
          assert(f.toSet == Set(("lunghezza",3.0),("muscolatura",2.0),("tutte",2.0)))
        case AllelicGeneStats(g,d,p,a,aq,f) if checkGeneEquality(g,gene2Seq)  =>
          assert(d == 3.0)
          assert(p == 1.0)
          assert(aq.toSet == Set(
            QualityType.FieldOfView,
            QualityType.Attractiveness)
          )
          assert(f == List(("view",3.0)))
        case AllelicGeneStats(g,d,p,a,aq,f) =>
        case _ => fail()
      }

  }

  test("Test retrieving of stats for sexual genes"){ geneticsSimulator =>
    var male = geneticsSimulator.newAnimal("Gatto")
    while(male.gender!=Male){
      male = geneticsSimulator.newAnimal("Gatto")
    }
    male.genome.sexualGeneCoupled.foreach{
      case (GeneInChromosome(t1,Some(g1)),GeneInChromosome(t2,None)) => {
        geneticsSimulator.getGeneStats(g1,male) match {
          case AllelicGeneStats(g,d,p,a,aq,f) if aq == List(QualityType.Fertility) =>
            assert(d == 5.0)
            assert(p == 1.0)
            assert(f.toSet == Set("fertility"->3.0))
          case AllelicGeneStats(g,d,p,a,aq,f) if aq == List(QualityType.Fecundity) =>
            assert(d == 5.0)
            assert(p == 1.0)
            assert(f.toSet == Set("fecundity"->3.0))
        }
      }
      case _=> fail()
    }

    var female = geneticsSimulator.newAnimal("Gatto")
    while(female.gender!=Female){
      female = geneticsSimulator.newAnimal("Gatto")
    }

    female.genome.sexualGeneCoupled.foreach {
      case (GeneInChromosome(t1, Some(g1)), GeneInChromosome(t2, Some(g2))) =>
      case _=> fail()
    }
  }
  def checkGeneEquality(g:MGene,al:Seq[ProteinoGenicAmminoacid]):Boolean = g.completeCode == al

  def geneStatsByChromosome(c:ChromosomeCouple#ChromosomeUnit,gs: GeneticsSimulator,a: AnimalInfo):Seq[GeneStats] =
    c.geneList.map(gs.getGeneStats(_,a))

}