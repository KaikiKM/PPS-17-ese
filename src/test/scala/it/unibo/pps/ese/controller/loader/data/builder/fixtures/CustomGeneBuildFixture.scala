package it.unibo.pps.ese.controller.loader.data.builder.fixtures

import it.unibo.pps.ese.controller.loader.beans.Allele
import it.unibo.pps.ese.controller.loader.data.CompleteAlleleData
import it.unibo.pps.ese.controller.loader.data.builder.{AlleleBuilder, GeneBuilder}

trait CustomGeneBuildFixture {
  def customGBFixture = new {

    private val all1 = AlleleBuilder()
      .setId("")
      .setGene("idcc")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    private val all2 = AlleleBuilder()
      .setId("")
      .setGene("idcsi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(1.0)

    private val all3 = AlleleBuilder()
      .setId("")
      .setGene("iddi")
      .setConsume(0)
      .setDominance(0)
      .setEffect(Map("test" -> 1))
      .setProbability(0.5)

    val complete = GeneBuilder()
      .setId("idcc")
      .setName("namecc")
      .addAlleles(Set(all1))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())

    val staticIncomplete = GeneBuilder()
      .setName("namecsi")
      .addAlleles(Set(all2))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())

    val dynamicIncomplete = GeneBuilder()
      .setId("iddi")
      .setName("namecdi")
      .addAlleles(Set(all3))
      .addProperties(Map("test" -> Double.getClass))
      .addConversionMap(Map())
  }
}
