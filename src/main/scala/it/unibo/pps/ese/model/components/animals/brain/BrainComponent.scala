package it.unibo.pps.ese.model.components.animals.brain

import java.util.Random

import it.unibo.pps.ese.controller.simulation.runner.core.EventBusSupport.{BaseEvent, RequestEvent, ResponseEvent}
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.MemoryType
import it.unibo.pps.ese.controller.simulation.runner.core._
import it.unibo.pps.ese.controller.simulation.runner.core.data.{EntityProperty, EntityState}
import it.unibo.pps.ese.controller.simulation.runner.core.support.SupervisedFuture
import it.unibo.pps.ese.controller.simulation.runner.incarnation.ReignType
import it.unibo.pps.ese.model.components._
import it.unibo.pps.ese.model.components.animals.DigestionEnd
import it.unibo.pps.ese.model.components.animals.brain.Direction.Direction
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport._
import it.unibo.pps.ese.model.components.animals.brain.decisionsupport.EntityAttributesImpl._
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus
import it.unibo.pps.ese.model.components.animals.brain.cerebralcortex.hippocampus.Hippocampus._
import it.unibo.pps.ese.model.components.animals.reproduction._
import it.unibo.pps.ese.utils.{Point, Position}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * Event with which inform the rest of the world of [[BrainComponent]] state.
  * @param strength the strength quality of the entity in subject
  * @param actionField the action field of the entity in subject
  * @param visualField the visual field of the entity in subject
  * @param attractiveness the attractiveness quality of the entity in subject
  */
case class BrainInfo(strength: Double,
                     actionField: Double,
                     visualField: Double,
                     attractiveness: Double) extends BaseEvent

/**
  * The enumeration that describes the type of possible actions.
  */
trait ActionTypes
case object Eat extends ActionTypes
case object Couple extends ActionTypes
case object Nothing extends ActionTypes

/**
  * It informs that the entity is using the hippocampus.
  */
case class UseHippocampus() extends BaseEvent

/**
  * It informs that the entity saw something that interests him.
  */
case class UseEyes() extends BaseEvent

/**
  * It informs about why the entity are moving.
  * (move because is searching for [[Eat]] or [[Couple]])
  * @param will the reason
  */
case class EntityWill(will: ActionTypes) extends BaseEvent

/**
  * It informs about the entity position.
  * @param position the entity position
  */
case class EntityPosition(position: Point) extends BaseEvent

/**
  * It informs that the entity is doing an action.
  * @param entityId the entity identifier
  * @param action the action
  */
case class InteractionEntity(entityId: String, action: ActionTypes) extends BaseEvent

/**
  * It informs that the brain requires its dynamic parameters.
  */
case class DynamicParametersRequest() extends RequestEvent

/**
  * This is the response of [[DynamicParametersRequest]] with the dynamic parameters.
  * @param id entity identifier
  * @param speed current speed
  * @param energy current energy
  * @param fertility current fertility
  * @param satisfaction current satisfaction
  */
case class DynamicParametersResponse(override val id: String, speed: Double, energy: Double, fertility: Double, satisfaction: Double) extends ResponseEvent

/**
  * Enumeration that defines the direction that the entity can take.
  */
object Direction extends Enumeration {
  type Direction = Value
  val RIGHT, LEFT, UP, DOWN, NONE = Value
}

/**
  * The component that is responsible for the decisions made (movements, hunting and finding partners)
  * @param entitySpecifications the base information of the entity
  * @param heightWorld the height of the world
  * @param widthWorld the width of the world
  * @param strength the strenght of the entity
  * @param actionField the action field of the entity
  * @param visualField the visual field of the entity
  * @param attractiveness the attractiveness of the entity
  * @param executionContext the execution context
  */
case class BrainComponent(override val entitySpecifications: EntitySpecifications,
                          heightWorld: Int,
                          widthWorld: Int,
                          strength: Double,
                          actionField: Double,
                          visualField: Double,
                          attractiveness: Double)
                         (implicit val executionContext: ExecutionContext) extends WriterComponent(entitySpecifications)  {

  /**
    * This function makes sure that any position that circulates in the world is within its limits.
    * The control is done automatically, implicitly, and in this way also converts a tuple to a point, so as to write a point in a simpler way.
    * @param tuple2 the tuple to convert and check
    * @return the point in the bounds
    */
  implicit def toPoint(tuple2: (Int, Int)): Point = {
    def bound(i: Int, bound: Int): Int = if (i < 0) 0 else if (i > bound) bound else i
    def boundWidth(x: Int): Int = bound(x, widthWorld)
    def boundHeight(y: Int): Int = bound(y, heightWorld)
    Point(boundWidth(tuple2._1), boundHeight(tuple2._2))
  }

  implicit def actionKindToMemoryType(actionKind: ActionTypes): MemoryType = actionKind match {
    case Eat => cerebralcortex.Hunting
    case Couple => cerebralcortex.Couple
  }

  implicit def pointToPosition(point: Point): Position = {
    Position(point.x, point.y)
  }

  /**
    * Minimum threshold of energy to ensure that an entity can go in search of partners instead of worrying about hunting a prey.
    */
  private val ENERGY_THRESHOLD = 60

  /**
    * Minimum of prey that must have an entity around it for can go in search of partners instead of worrying about hunting a prey.
    */
  private val MIN_PREYS_FOR_COUPLING = 2

  /**
    * Minimum threshold that must have an entity to be able to mate
    */
  private val FERTILITY_THRESHOLD = 0.4

  /**
    * Maximum threshold that must have an entity to be able to mate, otherwise it is satisfied.
    */
  private val SATISFACTION_THRESHOLD = 40

  /**
    * It takes into account whether it is in digestion or not.
    */
  private var digestionState: Boolean = false

  /**
    * It takes into account whether it is in pregnant state or not.
    */
  private var pregnantState: Boolean = false

  /**
    * A message to make sure that once the partner has been reached, the reproduction takes place.
    */
  private var forceReproduction: Option[ForceReproduction] = None

  /**
    * The element that allows to catalog the entities in radius and find out if there are prey or partners.
    */
  private val decisionSupport: DecisionSupport = DecisionSupport()

  private val hippocampus: Hippocampus = Hippocampus(widthWorld, heightWorld, visualField)

  /**
    * It stores the entities in its radius of vision.
    */
  private var entityInVisualField: Map[String, EntityAttributesImpl] = Map.empty

  override def initialize(): Unit = {
    subscribeEvents()
    configureMappings()
  }

  /**
    * Method by which events are received and the brain specifies the reaction to them.
    */
  private def subscribeEvents(): Unit = {
    import it.unibo.pps.ese.controller.simulation.runner.incarnation.EntityInfoConversion._
    subscribe {
      /*
      It means that an era has passed and the next move must be calculated.
       */
      case ComputeNextState() =>
        //If in the past era a shift occurred with the accomplished achievement of the partner, the next move is the coupling.
        if(forceReproduction.isDefined) {
          publish(ForceReproductionForward(forceReproduction.get))
          forceReproduction = None
          publish(new ComputeNextStateAck)
        }
        //Otherwise the dynamic parameters and the entities in your visual range are required in order to calculate the next best move.
        else {
          hippocampus.updateTime()

          val data = for {
            dynamicData <- requireData[DynamicParametersRequest, DynamicParametersResponse](new DynamicParametersRequest)
            baseInfo <- requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest)
            external <- requireData[EntitiesStateRequest, EntitiesStateResponse](EntitiesStateRequest(x => distanceBetween(x.state.position, baseInfo position) <= visualField))
          } yield (external, dynamicData)

          data onComplete{
            case Success((extData, dynData)) =>
              entityInVisualField = Map.empty
              //convert the representation of the entity of the world in the representation of the entity in decision support.
              extData.state map (x => convertToEntityAttributes(x)) foreach (x => entityInVisualField += (x.name -> x))
              //calculate the next move and communicate it.
              nextMove(dynData speed, dynData energy, dynData fertility, dynData satisfaction) onComplete (r => {
                publish(EntityPosition(r.get))
                publish(new ComputeNextStateAck)
              })
            case Failure(error) => throw error
          }

          /**
            * Convert an [[EntityState]] into [[EntityAttributesImpl]]
            * @param x the entity state
            * @return the entity attributes
            */
          def convertToEntityAttributes(x: EntityState): EntityAttributesImpl = if (x.state.reign == ReignType.ANIMAL) AnimalAttributes(x.entityId, x.state.species, x.state.height,
              x.state.strength, x.state.defense, (x.state.position.x, x.state.position.y),
              x.state.attractiveness, x.state.gender) else PlantAttributes(x.entityId, x.state.species, x.state.height, x.state.defense, (x.state.position.x, x.state.position.y), x.state.gender)
        }
      /*
      It tells the brain that in the next iteration he must mate because in the past era he has reached the partner.
      */
      case r: AutoForceReproduction =>
        forceReproduction = Some(r)
      /*
      It tells the brain that in the next iteration he must mate because in the past era someone has reached him.
      */
      case r: PartnerForceReproduction if r.receiverId == entitySpecifications.id =>
        forceReproduction = Some(r)
      /*
      It tells that the digestion phase ends.
      */
      case DigestionEnd() =>
        digestionState = false
      /*
      It tells that the pregnant phase starts.
      */
      case _: Pregnant =>
        pregnantState = true
      /*
      It tells that the pregnant phase ends.
      */
      case _: PregnancyEnd =>
        pregnantState = false
      /*
      If this message is received the brain has to communicate his information.
       */
      case GetInfo() =>
        publish(BrainInfo(strength, actionField, visualField, attractiveness))
        publish(new GetInfoAck)
      case _ => Unit
    }
  }

  /**
    * The exterior can see the component as a set of attributes.
    * These are modified by the published events.
    * This method defines how events change attributes.
    */
  private def configureMappings(): Unit = {
    addMapping[EntityPosition]((classOf[EntityPosition], ev => Seq(EntityProperty("position", ev position))))
    addMapping[ComputeNextState]((classOf[ComputeNextState], _ => Seq(EntityProperty("will", Nothing))))
    addMapping[EntityWill]((classOf[EntityWill], ev => Seq(EntityProperty("will", ev will))))
    addMapping[BrainInfo]((classOf[BrainInfo], ev => Seq(
      EntityProperty("strength", ev strength),
      EntityProperty("actionField", ev actionField),
      EntityProperty("visualField", ev visualField),
      EntityProperty("attractiveness", ev attractiveness),
      EntityProperty("will", Nothing)
    )))
  }

  /**
    * In this method the dynamic parameters are learned and based on these the brain decides what to do and where to move.
    * @param speed the current speed of the entity
    * @param energy the current energy of the entity
    * @param fertility the current fertility of the entity
    * @param satisfaction the current satisfaction of the entity
    * @return the new point
    */
  private def nextMove(speed: Double, energy: Double, fertility: Double, satisfaction: Double): SupervisedFuture[Point] = {
    requireData[BaseInfoRequest, BaseInfoResponse](new BaseInfoRequest) map (data => {
      //The current position is requested.
      var position = data position
      val floorSpeed = speed.toInt
      //The current field of vision is communicated to the decision support.
      val me: EntityAttributesImpl = AnimalAttributes(entitySpecifications id,
        EntityKinds(Symbol(data species)), data height, strength, data defense, (data.position.x, data.position.y),
        attractiveness, GenderTypes.withNameOpt(data gender).get)
      decisionSupport.createVisualField(entityInVisualField.values.toSeq :+ me)
      //The list of possible prey and partners is required for the decision support.
      val partners = decisionSupport.discoverPartners(me)
      val preys = decisionSupport.discoverPreys(me)
      var targets: Stream[EntityChoiceImpl] = preys
      var action: ActionTypes = Eat
      //if I have enough prey around and enough energy to not worry about eating in this iteration,
      // I have enough fertility to mate,
      // I do not have enough sexual satisfaction and I'm not pregnant
      // then I can mate
      if (energy > ENERGY_THRESHOLD
        && preys.lengthCompare(MIN_PREYS_FOR_COUPLING) > 0
        && fertility > FERTILITY_THRESHOLD
        && satisfaction < SATISFACTION_THRESHOLD
        && !pregnantState) {
        targets = partners; action = Couple
      }
      //If I am in a state of digestion, I do not move except to couple myself.
      if (action.equals(Couple) || (action.equals(Eat) && !digestionState)) {
        //I communicate my will.
        publish(EntityWill(action))
        //If I see something related of my will
        if (targets.nonEmpty) {
          //I communicate that
          publish(UseEyes())
          //and ask to the decision support the near prey/partner.
          val entityChoice = targets.min(Ordering.by((_:EntityChoiceImpl).distance))
          val entityAttribute = entityInVisualField(entityChoice.name)
          //I move towards the target,
          (0 until floorSpeed) foreach( _ => me.position = decisionSupport.nextMove(me, entityAttribute))
          //If the target entity is inside my action field
          if (entityChoice.distance < actionField) {
            //I do the action,
            me.position = entityAttribute.position
            publish(InteractionEntity(entityAttribute name, action))
            hippocampus.notifyEvent(action, Position(me.position.x, me.position.y))
            if (action.equals(Eat)) digestionState = true
          }
          //and update my position.
          position = Point(me.position.x, me.position.y)
        }
        //If I don't see anything related of my will
        else {
          position = hippocampus.searchingState match {
            /*
            No search has been started
             */
            case Inactive =>
              hippocampus.startNewSearch(action)
              publish(UseHippocampus())
              checkNewMemory
            /*
            A search has been started and not already ended
             */
            case Active =>
              publish(UseHippocampus())
              val d = hippocampus.computeDirection(position)
              val p = getPosition(d)
              p
            /*
            A search has been started and ended, no other memory is available
             */
            case Ended => getPosition(randomDirection)
          }

          /*
          Looks for a new memory, if present returns the next position towards it, else a new random position
           */
          def checkNewMemory: Point = {
            if (hippocampus.hasNewMemory) {
              hippocampus.chooseNewMemory(position)
              val d = hippocampus.computeDirection(position)
              val p = getPosition(d)
              p
            } else getPosition(randomDirection)
          }

          /*
          Returns a random direction
           */
          def randomDirection: Direction = {
            Direction(new Random().nextInt(Direction.values.size-1))
          }

          /**
            * It applies the direction to the current position.
            * @param direction direction to take
            * @return the updated position
            */
          def getPosition(direction: Direction): Point = direction match {
            case Direction.UP => (position.x, position.y - floorSpeed)
            case Direction.DOWN => (position.x, position.y + floorSpeed)
            case Direction.LEFT => (position.x - floorSpeed, position.y)
            case Direction.RIGHT => (position.x + floorSpeed, position.y)
            case Direction.NONE => checkNewMemory
          }
        }
      }
      //In all cases I clean the visual field of the decision support and return the updated position.
      decisionSupport.clearVisualField()
      position
    })
  }

  /**
    * Calculate the distance between two points.
    * @param from source point
    * @param to destination point
    * @return distance
    */
  private def distanceBetween(from: Point, to: Point) : Int =
    Math.sqrt(Math.pow(from.x - to.x, 2) + Math.pow(from.y - to.y, 2)).toInt

}