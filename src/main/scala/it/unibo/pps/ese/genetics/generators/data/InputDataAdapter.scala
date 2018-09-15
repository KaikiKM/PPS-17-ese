package it.unibo.pps.ese.genetics.generators.data

import it.unibo.pps.ese.controller.loader.data.AnimalData.CompleteAnimalData
import it.unibo.pps.ese.controller.loader.data.CustomGeneData.CompleteCustomGeneData
import it.unibo.pps.ese.controller.loader.data.DefaultGeneData.CompleteDefaultGeneData
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.genetics.dnaexpression.GeneData
import it.unibo.pps.ese.genetics.dnaexpression.AllelicData.AllelicInfoImpl
import it.unibo.pps.ese.genetics.dna.ProteinoGenicAmminoacid.ProteinoGenicAmminoacid
import it.unibo.pps.ese.genetics.entities.QualityType
import it.unibo.pps.ese.genetics.dnaexpression.{AlleleInfo, ConversionMap, Feature}
import it.unibo.pps.ese.genetics.dna.amminoAcidSeqFromString
private[genetics] object InputDataAdapter {
  implicit def translateAnimalData(aD:CompleteAnimalData):TranslatedAnimalData = {
    import ConversionUtilities._
    TranslatedAnimalDataImpl(
      aD.name,
      aD.geneLength,
      aD.reign,
      aD.typology,
      aD.structuralChromosome.map(customGeneDataToGeneData).toSeq,
      aD.regulationChromosome.map(defaultGeneDataToCustomGeneData).map(customGeneDataToGeneData).toSeq,
      aD.sexualChromosome.map(defaultGeneDataToCustomGeneData).map(customGeneDataToGeneData).toSeq
    )
  }
  private[this] object ConversionUtilities{
    def defaultGeneDataToCustomGeneData(dgd: CompleteDefaultGeneData):CompleteCustomGeneData ={
      object MyCustomGeneData extends CompleteCustomGeneData {

        override def conversionMap: Map[String, Map[String, Double]] = Map(dgd.name->Map(dgd.name->1.0))
        override def id: String = dgd.id

        override def name: String = dgd.name

        override def properties: Map[String, Class[_]] = dgd.properties

        override def alleles: Set[CompleteAlleleData] = dgd.alleles

        override def getConversionMap: Option[Map[String, Map[String, Double]]] = None

        //TODO can't extend my interface
        override def getId: Option[String] = None

        override def getProperties: Option[Map[String, Class[_]]] = None

        override def getAlleles: Option[Set[CompleteAlleleData]] = None
      }
      MyCustomGeneData
    }

    def customGeneDataToGeneData(customGeneData: CompleteCustomGeneData):GeneData = {
      def getQualityTypeByString(s:String):QualityType = {
        QualityType.values.find(q=>q.entryName.toLowerCase()==s.toLowerCase).get
      }
      val geneSeq:Seq[ProteinoGenicAmminoacid] = amminoAcidSeqFromString(customGeneData.id)

      def getConversionMapSeq(map:Map[String,Double]):Seq[ConversionMap]  ={
        map.map(e=>ConversionMap(getQualityTypeByString(e._1),e._2)).toSeq
      }
      def getFeatureByName(s:String,seq: Seq[Feature]):Feature = {
        seq.find(_.name==s).get
      }
      val geneFeatures:Seq[Feature]=customGeneData.conversionMap.map(cMap=>{
        Feature(cMap._1,getConversionMapSeq(cMap._2))
      }).toSeq

      val allelicForms:Seq[AlleleInfo] = customGeneData.alleles.toSeq.map(alleleData=>{
        val featuresBehaviour:Seq[(Feature,Double)] = alleleData
          .effect
          .map(e=>(getFeatureByName(e._1,geneFeatures),e._2))
          .toSeq
        new AllelicInfoImpl(
          geneSeq,
          amminoAcidSeqFromString(alleleData.id),
          alleleData.dominance.toInt,
          featuresBehaviour,
          alleleData.consume,
          alleleData.probability
        )
      }
      )
      GeneData(
        geneSeq,
        customGeneData.name,
        geneFeatures,
        allelicForms
      )
    }
  }

}
