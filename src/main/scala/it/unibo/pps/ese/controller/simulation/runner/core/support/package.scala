package it.unibo.pps.ese.controller.simulation.runner.core

import java.util.UUID.randomUUID

package object support {

  trait BaseEvent
  trait HighPriorityEvent
  trait RequestEvent extends BaseEvent { val id : String = randomUUID().toString }
  trait ResponseEvent extends BaseEvent { val id : String }
  trait InteractionEvent extends BaseEvent { val receiverId: String }

  case class InteractionEnvelope[A](sourceId : String, targetId: String, message: A)
  class Done

  case class IdentifiedEvent(sourceId: String, event: Event)
  case class IdentifiedConsumer(sourceId: String, consumer: Event => Unit)

  type Consumer = IdentifiedConsumer
  type RequestConsumer = RequestEvent => Unit
  type Event = BaseEvent

}