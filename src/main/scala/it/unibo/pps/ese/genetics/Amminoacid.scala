package it.unibo.pps.ese.genetics

object ProteinoGenicAmminoacid extends Enumeration {
    type ProteinoGenicAmminoacid = Value
    protected case class Val(shortName: Char, name: String) extends super.Val{
      override def toString(): String =super.toString()+ ": { "+shortName+", "+name+" }"
    }

    implicit def valueToAmminoacidVal(x: Value): Val = x.asInstanceOf[Val]
    val Ala = Val('A', "Alanine")
    val Cys = Val('C',"Cysteine")
    val Asp = Val('D',"Aspartic acid")
    val Glu = Val('E',"Glutamic acid")
    val Phe = Val('F',"Phenyalanine")
    val Gly = Val('G',"Glycine")
    val His = Val('H',"Histidyne")
    val Lys = Val('K',"Lysine")
    val Leu = Val('L',"Leucine")
    val Met = Val('M',"Methionine")
    val Asn = Val('N',"Asparagine")
    val Pyl = Val('O',"Pyrrolisine")
    val Pro = Val('P',"Proline")
    val Gln = Val('Q',"Glutamine")
    val Arg = Val('R',"Arginine")
    val Ser = Val('S',"Serine")
    val Thr = Val('T',"Threonine")
    val Sec = Val('U',"Selenocysteine")
    val Vali = Val('V',"Valine")
    val Trp = Val('W',"Tryptophan")
    val Tyr = Val('Y',"Tyrosine")
}
  import ProteinoGenicAmminoacid._
  object AmminoAcidUtilities {
    implicit def charToAmminoacid(c:Char):ProteinoGenicAmminoacid = {
      ProteinoGenicAmminoacid.values.find(p=>p.shortName == c).getOrElse(throw new IllegalArgumentException)
    }
    implicit def seqCharToListAmminoacid(seq: Seq[Char]):Seq[ProteinoGenicAmminoacid] = {
      seq.map(charToAmminoacid)
    }
  }
