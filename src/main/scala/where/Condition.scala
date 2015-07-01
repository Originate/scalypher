package com.originate.scalypher.where

import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.PropertyName
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.path.AnyNode
import scala.language.implicitConversions

sealed trait Condition {
  def toQuery(identifiableMap: ReferenceableMap): String
  def identifiables: Set[Identifiable]
}

object Condition {
  implicit def toWhere(condition: Condition): Where =
    Where(condition)

  implicit def optionToWhere(condition: Option[Condition]): Option[Where] =
    condition map (Where(_))
}

case class Comparison(reference1: Reference, comparator: Comparator, reference2: Reference) extends Condition {
  def toQuery(identifiableMap: ReferenceableMap): String =
    Seq(reference1.toQuery(identifiableMap), comparator.toQuery, reference2.toQuery(identifiableMap)) mkString " "

  def identifiables: Set[Identifiable] =
    Set(reference1, reference2) flatMap (_.getReferenceable)
}

case class NullCondition(reference: Reference, check: NullCheck) extends Condition {
  def toQuery(identifiableMap: ReferenceableMap): String =
    Seq(reference.toQuery(identifiableMap), check.toQuery) mkString " "

  def identifiables: Set[Identifiable] =
    reference.getReferenceable.toSet
}

case class PredicateCondition(
  predicate: Predicate,
  projection: Collection,
  where: ObjectReference => Where
) extends Condition {
  def toQuery(identifiableMap: ReferenceableMap): String = {
    val identifier = "x"
    val identifiable = AnyNode()
    val adjustedMap = identifiableMap + (identifiable -> identifier)
    val conditionString = where(ObjectReference(identifiable)).toQuery(adjustedMap)

    Seq(
      predicate.toQuery,
      s"($identifier IN ${projection.toQuery(identifiableMap)} WHERE $conditionString)"
    ) mkString " "
  }

  def identifiables: Set[Identifiable] =
    projection.identifiables
}

case class Expression(string: String, references: Reference*) extends Condition {
  def toQuery(identifiableMap: ReferenceableMap): String = {
    val questionMarksCount = (string filter (_ == '?')).size
    if (questionMarksCount != references.size)
      throw new MismatchedInterpolatedStringWithReferences(string, questionMarksCount, references.size)
    else {
      val expression = references.foldLeft(string) { (acc, reference) =>
        // avoiding replaceFirst because it has special handling of escape characters:
        // http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceFirst%28java.lang.String,%20java.lang.String%29
        val pieces = acc.split("[?]", 2)
        Seq(pieces.lift(0), Some(reference.toQuery(identifiableMap)), pieces.lift(1)).flatten mkString ""
      }
      s"($expression)"
    }
  }

  def identifiables: Set[Identifiable] =
    (references flatMap (_.getReferenceable)).toSet
}
