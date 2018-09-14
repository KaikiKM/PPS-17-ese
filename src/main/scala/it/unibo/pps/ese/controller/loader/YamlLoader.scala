package it.unibo.pps.ese.controller.loader

import java.io
import java.io.InputStream

import it.unibo.pps.ese.controller.loader.beans._
import it.unibo.pps.ese.controller.loader.data.AnimalData.PartialAnimalData
import it.unibo.pps.ese.controller.loader.data.SimulationData.PartialSimulationData
import it.unibo.pps.ese.controller.loader.data._
import it.unibo.pps.ese.controller.loader.data.builder._
import it.unibo.pps.ese.controller.util.io.File.FileFormats
import it.unibo.pps.ese.controller.util.io.{ExistingResource, File, Folder, IOResource}
import net.jcazevedo.moultingyaml._
import org.kaikikm.threadresloader.ResourceLoader


object YamlLoader extends Loader {

  object CustomYaml extends DefaultYamlProtocol {
    implicit val simulationFormat: YamlFormat[Simulation] = yamlFormat2(Simulation)
    implicit val plantFormat: YamlFormat[Plant] = yamlFormat9(Plant)
    implicit val defaultChromosomeDataFormat: YamlFormat[DefaultChromosomeData] = yamlFormat2(DefaultChromosomeData)
    implicit val animalFormat: YamlFormat[Animal] = yamlFormat8(Animal)
    implicit val propertyInfoFormat: YamlFormat[PropertyInfo] = yamlFormat1(PropertyInfo)
    implicit val geneFormat: YamlFormat[Gene] = yamlFormat4(Gene)
    implicit val alleleFormat: YamlFormat[Allele] = yamlFormat6(Allele)
  }

  import CustomYaml._

  implicit val int: DefaultValue[Int] = DefaultValue(Integer.MIN_VALUE)
  implicit val double: DefaultValue[Double] = DefaultValue(Double.MinValue)
  implicit val string: DefaultValue[String] = DefaultValue("")
  implicit val iterable: DefaultValue[Iterable[_]] = DefaultValue(Iterable())

  override def loadSimulation(configPath: String): PartialSimulationData = {
    //TODO not only classpath
    val simConfig = File(ResourceLoader.getResource(configPath))
    val currentFolder = simConfig.getParentFolder().get
    val simulation = loadFileContent(simConfig).parseYaml.convertTo[Simulation]

    var builder : SimulationBuilder[_] = SimulationBuilder()
    if(simulation.animals.isDefined) {
      val animals: Map[PartialAnimalData, Int] = simulation.animals.get.map({
        case (animalConfigPath, v) =>
          val animal: PartialAnimalData = normalizeConfigPath(animalConfigPath, currentFolder) match {
            case f: File =>
              loadAnimal(f)
          }
          val ret: (PartialAnimalData, Int) = (animal, v)
          ret
      })
      builder = builder.addAnimals(animals)
    }
    if(simulation.plants.isDefined) {
      val plants: Map[PartialPlantData, Int] = simulation.plants.get.map({
        case (plantConfigPath, v) =>
          val plant: PartialPlantData = normalizeConfigPath(plantConfigPath, currentFolder) match {
            case f: File =>
              loadPlant(f)
          }
          val ret: (PartialPlantData, Int) = (plant, v)
          ret
      })
      builder = builder.addPlants(plants)
    }
    builder.build
  }

  private def loadPlant(config: File): PartialPlantData = {
    val loadedPlant = loadFileContent(config).parseYaml.convertTo[Plant]
    var builder: PlantBuilder[_] = PlantBuilder().setName(loadedPlant.name)
    if(loadedPlant.reign.isDefined)
      builder = builder.setReign(loadedPlant.reign.get)
    if(loadedPlant.alleleLength.isDefined)
      builder = builder.setAlleleLength(loadedPlant.alleleLength.get)
    if(loadedPlant.geneLength.isDefined)
      builder = builder.setGeneLength(loadedPlant.geneLength.get)
    if(loadedPlant.attractiveness.isDefined)
      builder = builder.setAttractiveness(loadedPlant.attractiveness.get)
    if(loadedPlant.hardness.isDefined)
      builder = builder.setHardness(loadedPlant.hardness.get)
    if(loadedPlant.availability.isDefined)
      builder = builder.setAvailability(loadedPlant.availability.get)
    if(loadedPlant.height.isDefined)
      builder = builder.setHeight(loadedPlant.height.get)
    if(loadedPlant.nutritionalValue.isDefined)
      builder = builder.setNutritionalValue(loadedPlant.nutritionalValue.get)
    builder.build()
  }

  private def loadAnimal(config: File): PartialAnimalData = {
    val loadedAnimal = loadFileContent(config).parseYaml.convertTo[Animal]
    var structuralChromosome: Seq[GeneBuilder[_]] = Seq()
    var regulationChromosome: Seq[GeneBuilder[_]] = Seq()
    var sexualChromosome: Seq[GeneBuilder[_]] = Seq()
    if(loadedAnimal.structuralChromosome.isDefined)
      structuralChromosome = normalizeConfigPath(loadedAnimal.structuralChromosome.get, config.getParentFolder().get) match {
        case f: Folder =>
          loadStructuralChromosome(f)
      }
    if(loadedAnimal.regulationChromosome.isDefined)
      regulationChromosome = loadDefaultChromosome(RegulationDefaultGenes.elements, loadedAnimal.regulationChromosome.get, config.getParentFolder().get)
    if(loadedAnimal.sexualChromosome.isDefined)
      sexualChromosome = loadDefaultChromosome(SexualDefaultGenes.elements, loadedAnimal.sexualChromosome.get, config.getParentFolder().get)
    var builder: AnimalBuilder[_] = AnimalBuilder().setName(loadedAnimal.name)
    if(loadedAnimal.typology.isDefined)
      builder = builder.setTypology(loadedAnimal.typology.get)
    if(loadedAnimal.reign.isDefined)
      builder = builder.setReign(loadedAnimal.reign.get)
    if(loadedAnimal.alleleLength.isDefined)
      builder = builder.setAlleleLength(loadedAnimal.alleleLength.get)
    if(loadedAnimal.geneLength.isDefined)
      builder = builder.setGeneLength(loadedAnimal.geneLength.get)
    if(structuralChromosome.isValid)
      builder = builder.addStructuralChromosome(structuralChromosome)
    if(regulationChromosome.isValid)
      builder = builder.addRegulationChromosome(regulationChromosome)
    if(sexualChromosome.isValid)
      builder = builder.addSexualChromosome(sexualChromosome)
    builder.build
  }

  private def loadDefaultChromosome[T <: DefaultGene](genesSet: Set[T], chromosomeData: DefaultChromosomeData,
                                                      currentFolder: Folder): Seq[GeneBuilder[_]] = {
    //TODO in builder, only check subset here
    //require(chromosomeData.names.keySet == genesSet.map(_.name))
    var alleles: Seq[AlleleBuilder[_]] = Seq()
    if(chromosomeData.allelesPath.isDefined) {
      alleles = normalizeConfigPath(chromosomeData.allelesPath.get, currentFolder) match {
        case f: Folder =>
          loadAlleles(f)
      }
    }
    //TODO check no wrong alleles
    chromosomeData.names.getOrElse(Seq()).toSeq.map({
      case (k, v) =>
        var builder: GeneBuilder[_] = GeneBuilder()
          .setDefaultInfo(genesSet.find(e => e.name == k).get)
        if(v.isValid)
          builder = builder.setId(v)
        if(chromosomeData.allelesPath.isDefined && v.isValid)
          builder = builder.addAllelesB(alleles.filter(a => a.gene.getOrElse("") == v))
        builder
    })
  }

  private def loadStructuralChromosome(genesFolder: Folder): Seq[GeneBuilder[_]] =  {
    genesFolder.getFilesAsStream(FileFormats.YAML)
      .map(loadFileContent(_).parseYaml.convertTo[Gene])
      .map(g => {
        var builder: GeneBuilder[_] = GeneBuilder().setName(g.simpleName)
        if(g.id.isDefined)
          builder = builder.setId(g.id.get)
        if(g.properties.isDefined)
          builder = builder.setCustomProperties(g.properties.get)
        if(g.allelesPath.isDefined) {
          val alleles = normalizeConfigPath(g.allelesPath.get, genesFolder) match {
            case f: Folder =>
              loadAlleles(f)
          }
          builder = builder.addAllelesB(alleles)
        }
        builder
      })
  }

  private def loadAlleles(allelesFolder: Folder): Seq[AlleleBuilder[_]] = {
    allelesFolder.getFilesAsStream(FileFormats.YAML)
      .map(path => {
        val all = loadFileContent(path).parseYaml.convertTo[Allele]
        var builder: AlleleBuilder[_] = AlleleBuilder().setId(all.id)
        if(all.gene.isDefined)
          builder = builder.setGene(all.gene.get)
        if(all.consume.isDefined)
          builder = builder.setConsume(all.consume.get)
        if(all.dominance.isDefined)
          builder = builder.setDominance(all.dominance.get)
        if(all.effect.isDefined)
          builder = builder.setEffect(all.effect.get)
        if(all.probability.isDefined)
          builder = builder.setProbability(all.probability.get)
        builder
      })
  }

  private def normalizeConfigPath(path: String, currentFolder: Folder): ExistingResource = {
    if(path.startsWith("./")) {
      currentFolder.getExistingChildren(path.drop(2)).get
    } else {
      IOResource(path) match {
        case r: ExistingResource =>
          r
        case _ =>
          println(path)
          throw new IllegalStateException()
      }
    }
  }


  private def loadFileContent(file: File): String = {
    loadFileContent(file.openInputStream)
  }

  private def loadFileContent(input: InputStream): String = {
    val source = scala.io.Source.fromInputStream(input)
    val ret = source.mkString
    source.close()
    ret
  }

  case class DefaultValue[T](get: T)

  trait Validable[T] {
    def isValid(implicit defaultValue: DefaultValue[T]): Boolean
  }

  implicit class ValidableString(str: String) extends Validable[String] {
    def isValid(implicit defaultValue: DefaultValue[String]): Boolean = str != defaultValue.get
  }

  implicit class ValidableIterable(it: Iterable[_]) extends Validable[Iterable[_]] {
    def isValid(implicit defaultValue: DefaultValue[Iterable[_]]): Boolean = it != defaultValue.get
  }

  implicit class ValidableNumeric(num: Double) extends Validable[Double] {
    def isValid(implicit defaultValue: DefaultValue[Double]): Boolean = num != defaultValue.get
  }

  trait DefaultGet[T] {
    def getOrDefault(implicit defaultValue: DefaultValue[T]): T
  }
}