package it.unibo.pps.ese.controller.simulation.runner.incarnation.coordinators

import it.unibo.pps.ese.controller.simulation.loader.{Saver, YamlLoader, YamlSaver}
import it.unibo.pps.ese.controller.simulation.loader.data.SimulationData.{CompleteSimulationData, PartialSimulationData}
import it.unibo.pps.ese.controller.simulation.loader.io.{ExistingResource, File, Folder}
import it.unibo.pps.ese.controller.simulation.runner.incarnation.SimulationBuilder
import it.unibo.pps.ese.controller.simulation.runner.incarnation.SimulationBuilder.Simulation.EmptySimulation

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait Controller {
  def startSimulation(file: File): Future[Try[(SimulationController, CompleteSimulationData)]]
  def startSimulation(data: CompleteSimulationData): Future[Try[SimulationController]]
  def loadSimulation(file: File): Future[Try[PartialSimulationData]]
  def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]]
  def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]]
}
object Controller {

  def apply()(implicit executionContext: ExecutionContext): Controller = new BaseController()

  private class BaseController()(implicit executionContext: ExecutionContext) extends Controller {

    var saver: Option[Saver] = None

    override def startSimulation(data: CompleteSimulationData): Future[Try[SimulationController]] = {
      Future(Success(new SimulationBuilder[EmptySimulation]
        .dimension(500, 500)
        .data(data)
        .build))
    }

    override def startSimulation(file: File): Future[Try[(SimulationController, CompleteSimulationData)]] = {
      Future(YamlLoader.loadCompleteSimulation(file) match {
        case Success(value) =>
          Success((new SimulationBuilder[EmptySimulation]
            .dimension(500, 500)
            .data(value)
            .build, value))
        case Failure(exception) =>
          Failure(exception)
      })
    }

    override def loadSimulation(file: File): Future[Try[PartialSimulationData]] = {
      Future(Try(YamlLoader.loadSimulation(file)))
    }

    override def saveSimulationData(simulation: PartialSimulationData, simulationName: String, target: Folder): Future[Try[Unit]] = {
      Future({
        saver = Some(YamlSaver(simulation, simulationName))
        saver.get.saveData(target, false)
      })
    }

    override def retrySave(target: Folder, overrideResource: Option[ExistingResource], overrideAll: Boolean = false): Future[Try[Unit]] = {
      Future(saver match {
        case Some(s) =>
          overrideResource.foreach(s.addResourceToOverride)
          s.saveData(target, overrideAll)
        case None =>
          Failure(new IllegalStateException())
      })
    }
  }
}
