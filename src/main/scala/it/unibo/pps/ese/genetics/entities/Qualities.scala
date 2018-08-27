package it.unibo.pps.ese.genetics.entities

import enumeratum._

  sealed trait QualityType extends EnumEntry
  object QualityType extends Enum[QualityType]{
    val values = findValues
    case object Speed extends QualityType
    case object FieldOfView extends QualityType
    case object RangeOfAction extends QualityType
    case object Life extends QualityType
    case object EnergyRequirements extends QualityType
    case object Height extends QualityType
    case object Strength extends QualityType
    case object ResistenceToAttack extends QualityType
    case object NutritionalValue extends QualityType
    case object Fertility extends QualityType
    case object Fecundity extends QualityType
    case object Attractiveness extends QualityType
    case object Childhood extends QualityType
    case object Maturity extends QualityType
    case object Oldness extends QualityType
    case object Decline extends QualityType
    case object Availability extends QualityType
    case object Hardness extends QualityType
    def maleSexualQualities:Seq[QualityType] = List(Fertility)
    def plantQualities:Seq[QualityType] = List(Availability,Attractiveness,Height,NutritionalValue,Hardness)
    def femaleSexualQualities:Seq[QualityType] = List(Fertility,Fecundity)
  }


  sealed trait Quality{
    def qualityValue:Double
    def qualityType:QualityType
  }

  object Quality{
    def apply(qualityValue:Double,qualityType: QualityType) = QualityImpl(qualityValue,qualityType)
    private [Quality] case class QualityImpl(
                            override val qualityValue:Double,
                            override val qualityType: QualityType) extends Quality {
      import QualityValueConstraints._
      require(checkParamater(qualityType,qualityValue))
      def checkParamater(f:Double=>Boolean,v:Double):Boolean = f(v)
      override def toString: String = qualityType.toString+": "+qualityValue
    }
  }

  object QualityValueConstraints{
    implicit def qualityTypeToConstraint(qualityType: QualityType):Double=>Boolean = {
      if(constraints.contains(qualityType)) constraints(qualityType) else (_)=>true
    }
    val maxSpeed:Double= 100.0
    val speedConstraints:Double=>Boolean = s => s>0.0 && s < maxSpeed
    val constraints:Map[QualityType,Double=>Boolean] = Map(
      QualityType.Speed->speedConstraints,
    )

  }
