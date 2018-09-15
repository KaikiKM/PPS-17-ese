package it.unibo.pps.ese.view.bodyViewer

import it.unibo.pps.ese.entitybehaviors.EmbryoStatus

private[bodyViewer] object OrganInfoPrinter{
  def getHeadText(brainStatus: BrainStatus,eyesStatus: EyesStatus):String
  =(brainStatus,eyesStatus) match {
    case (HippoCampusActive(r),EyesDisabled) =>
      r match {
        case Evaluating =>
          "I can not see anything in my field" +
            " of vision and I'm thinking about what to do"
        case Coupling =>
          "I can not find a partner in my field of vision " +
            "but I'm trying to remember where I saw one"
        case Eating =>
          "I can not find a source of food in my field of vision " +
            "but I'm trying to remember where I saw one"
      }
    case (HippoCampusDisabled,EyesActive(r)) =>
      r match {
        case Evaluating =>
          "I'm looking around to decide what to do"
        case Coupling =>
          "I would like to reproduce myself and I have just identified a possible partner, great!"
        case Eating =>
          "I'm hungry and I just spotted something to feed myself"
      }
    case (HippoCampusDisabled,EyesDisabled)=>
      "I have nothing to do..."
    case _=>""
  }
  def getDigestiveSystemStatus(digestiveSystemStatus: DigestiveSystemStatus):String=
    digestiveSystemStatus match {
      case Digesting => "What a good meal, I'm digesting..."
      case NotDigesting => "I have nothing to digest..."
    }
  def getReproductiveSystemStatus(
                                   reproductiveApparatusStatus: ReproductiveApparatusStatus,
                                   embryoStatus: Option[EmbryoStatus.Value]):String=
    (reproductiveApparatusStatus,embryoStatus) match {
      case (Reproducing,None) => "I found the right partner for me so I will copulate"
      case (NotReproducing,None) => "I have not found the right partner yet"
      case (NotReproducing,Some(v)) =>
        val embryoText = v match {
          case EmbryoStatus.primal => " and my gestation is a the beginning"
          case EmbryoStatus.mid => " and my gestation is half-done"
          case EmbryoStatus.advanced => " and my gestation is a almost finished"
        }
        "Oh...I'm pregnant"+embryoText
      case _=>"No Description"
    }
}