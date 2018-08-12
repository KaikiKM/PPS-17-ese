package it.unibo.pps.ese.genetics

import AmminoAcidUtilities._
import it.unibo.pps.ese.genetics.DnaTranslator.DnaTranslatorImpl

sealed trait SpeciesUtilities{
  def generateAnimalGenome:AnimalGenome
  def translateGenome(genome:AnimalGenome):AnimalInfo
  def obtainMutantAlleles:Seq[AlleleInfo]
}
object SpeciesUtilities{
  def apply(animalData:TranslatedAnimalData):SpeciesUtilities = new SpeciesSetup(animalData)
  private class SpeciesSetup(animalData: TranslatedAnimalData) extends SpeciesUtilities {
    private val allGeneData:Seq[GeneData] = animalData.structuralChromosome ++
      animalData.regulationChromosome ++
      animalData.sexualChromosome

    private val geneFeatures:Seq[GeneFeatures] =allGeneData.map(geneData=>{
      val allelicBehaviours:Seq[AllelicBehaviour] = geneData
        .allelicForm
        .map(ad=>AllelicBehaviour(
          ad.geneSeq,
          ad.allelicSeq,
          ad.dominanceLevel,
          ad.featuresBehaviour,
          ad.energyConsumption))
      GeneFeatures(geneData.geneSeq,geneData.name,geneData.geneFeatures,allelicBehaviours)
    })

    private val dnaTranslator:DnaTranslator = new DnaTranslatorImpl(geneFeatures)

    private val speciesGenerator:SpeciesGenerator = new SpeciesGenerator(
      commonChromosomeGenes = List(stringToReign(animalData.reign),speciesNameToGene(animalData.name)),
      structuralChromosomeGenes = allGenes(animalData.structuralChromosome),
      lifeCycleChromosomeGenes = allGenes(animalData.regulationChromosome),
      feedingChromosomeGenes = List(stringToDiet(animalData.typology)),
      sexualChromosomeGenes = allGenes(animalData.sexualChromosome)
    )
    private val mutantAllele:Seq[AlleleInfo] = allGeneData.flatMap(_.allelicForm).filter(_.probability==0)

    private def stringToReign(s:String):BasicGene = {
      val aS:String = Animal.reignName
      val pS:String = Plant.reignName
      s match {
        case `aS` => Animal.geneId
        case `pS` => Plant.geneId
      }
    }

    private def speciesNameToGene(s:String):BasicGene = {
      BasicGene(amminoAcidSeqFromString(s),IdentifierGene)
    }
    private def allGenes(genes:Seq[GeneData]):Seq[GeneWithPossibleAlleles] = {
      genes.map(geneData=>{
        val alleles:Seq[AlleleWithProbability] = geneData
          .allelicForm
          .filter(_.probability>0)
          .map(allele=>
            AlleleWithProbability(allele.allelicSeq,allele.probability))
        GeneWithPossibleAlleles(geneData.geneSeq,alleles)
      })
    }

    private def stringToDiet(s:String):BasicGene = {
      val eS:String = Herbivore.dietName
      val cS:String = Carnivorous.dietName
      s match {
        case `eS` => Herbivore.geneId
        case `cS` => Carnivorous.geneId
      }
    }

    override def generateAnimalGenome: AnimalGenome = speciesGenerator.generateAnimalGenome

    override def translateGenome(genome:AnimalGenome): AnimalInfo = AnimalInfo(
      dnaTranslator.getQualitiesByGenome(genome),
      genome
    )

    override def obtainMutantAlleles: Seq[AlleleInfo] = mutantAllele
  }
}

