package cc.factorie.app.bib.hcoref

import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.OptionUtils
import edu.umass.cs.iesl.scalacommons.StringUtils
import scala.collection.mutable

/**
 * @author John Sullivan
 */
object Canopies {

  def fromString(rawNameString:String):String = StringUtils.toOptionNonempty(rawNameString.trim.replace(", , ",", ")) match {
    case Some(nonEmptyNameString) =>
      try{
        val personName = PersonNameWithDerivations(nonEmptyNameString).inferFully
        val res = OptionUtils.merge[NonemptyString](personName.firstInitial, personName.longestSurName, {(initial, surname) => NonemptyString(initial.s.substring(0, 1).toLowerCase + surname.s)}).map(_.s).getOrElse("")
        res
      } catch {
        case e:Exception =>
          println("Failed on %s" format rawNameString)
          //e.printStackTrace()
          ""
      }
    case None => ""
  }

}
