package it.unibo.pps.ese.genetics

object Qualities {
  sealed trait QualityType
  case object Speed extends QualityType
  case object FieldOfView extends QualityType
  case object RangeOfAction extends QualityType
  case object MaxLife extends QualityType
  case object EnergyRequirements extends QualityType
  case object Height extends QualityType
  case object ResistenceToAttack extends QualityType
  case object DietType extends QualityType
  case object NutritionalValue extends QualityType
  case object Fertility extends QualityType
  case object Fecondity extends QualityType
  case object Attractiveness extends QualityType
  case object ChildhoodLenght extends QualityType
  case object AdultnessLenght extends QualityType
  case object PercentageDecay extends QualityType


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
    }
  }


  object QualityValueConstraints{
    implicit def qualityTypeToConstraint(qualityType: QualityType):Double=>Boolean = {
      constraints(qualityType)
    }
    val maxSpeed:Double= 100.0
    val speedConstraints:Double=>Boolean = s => s>0.0 && s < maxSpeed
    val constraints:Map[QualityType,Double=>Boolean] = Map(
      Speed->speedConstraints,
    )

  }
}
