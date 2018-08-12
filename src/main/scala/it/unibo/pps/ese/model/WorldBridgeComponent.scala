package it.unibo.pps.ese.model

import it.unibo.pps.ese.model.support.{BaseEvent, Done}

import scala.concurrent.{Future, Promise}

case class RequireEntitiesState(id: String, filter: EntityState => Boolean = _ => true) extends BaseEvent
case class EntitiesStateResponse(id: String, state: Seq[EntityState]) extends BaseEvent
case class CreateEntity(entity: Entity) extends BaseEvent
case class KillEntity(entityId: String) extends BaseEvent
case class NewState(properties: Seq[EntityProperty]) extends BaseEvent
case class ComputeNextState() extends BaseEvent
case class ComputeNextStateResponse() extends BaseEvent
case class GetInfo() extends BaseEvent
case class GetInfoResponse() extends BaseEvent

sealed trait WorldBridge {
  def computeNewState(): Future[Done]
  def requireInfo(): Future[Done]
}

class WorldBridgeComponent(override val entitySpecifications: EntitySpecifications, world: InteractiveWorld) extends WriterComponent(entitySpecifications) with WorldBridge {

  private var newStatePromise : Promise[Done] = Promise()
  private var requireInfoPromise : Promise[Done] = Promise()
  private var newStateAccumulator : Long = 0
  private var requireInfoAccumulator : Long = 0
  private var componentsNumber : Long = 0

  override def initialize(): Unit = subscribe {
    case RequireEntitiesState(id, filter) => publish(EntitiesStateResponse(id, (world queryableState) getFilteredState filter))
    case CreateEntity(entity) => world addEntity entity
    case KillEntity(entityId) => world removeEntity entityId
    case NewState(properties) => properties foreach (e => (world queryableState) addOrUpdateEntityState (entitySpecifications id, e))
    case ComputeNextStateResponse() =>
      newStateAccumulator += 1
      if (newStateAccumulator == entitySpecifications.componentsCount) newStatePromise success new Done()
    case GetInfoResponse() =>
      requireInfoAccumulator += 1
      if (requireInfoAccumulator == entitySpecifications.componentsCount) requireInfoPromise success new Done()
    case _ => Unit
  }

  override def computeNewState(): Future[Done] = {
    if (newStatePromise isCompleted) {
      publish(ComputeNextState())
      newStatePromise = Promise()
      newStateAccumulator = 0
      newStatePromise future
    } else {
      failurePromise()
    }
  }

  override def requireInfo(): Future[Done] = {
    if (requireInfoPromise isCompleted) {
      publish(GetInfo())
      requireInfoPromise = Promise()
      requireInfoAccumulator = 0
      requireInfoPromise future
    } else {
      failurePromise()
    }
  }

  private def failurePromise() : Future[Done] = {
    val result = Promise[Done]()
    result failure new RuntimeException("Already pending request")
    result future
  }
}
