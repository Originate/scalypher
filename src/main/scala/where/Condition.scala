package com.scalypher.where

import com.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.scalypher.PropertyReference
import com.scalypher.types.Referenceable
import com.scalypher.types.ReferenceableMap
import com.scalypher.path.AnyNode
import scala.language.implicitConversions

sealed trait Condition {
  def toQuery(referenceableMap: ReferenceableMap): String
  def referenceables: Set[Referenceable]
}

object Condition {
  implicit def toWhere(condition: Condition): Where =
    Where(condition)
}

case class Comparison(reference1: ReferenceType, comparator: Comparator, reference2: ReferenceType) extends Condition {
  def toQuery(referenceableMap: ReferenceableMap): String =
    Seq(reference1.toQuery(referenceableMap), comparator.toQuery, reference2.toQuery(referenceableMap)) mkString " "

  def referenceables: Set[Referenceable] =
    Set(reference1, reference2) flatMap (_.getReferenceable)
}

case class Check(reference: ReferenceType, check: CheckType) extends Condition {
  def toQuery(referenceableMap: ReferenceableMap): String =
    Seq(reference.toQuery(referenceableMap), check.toQuery) mkString " "

  def referenceables: Set[Referenceable] =
    reference.getReferenceable.toSet
}

case class PredicateCondition(predicate: Predicate, projection: Collection, where: AnyNode => Condition) extends Condition {
  def toQuery(referenceableMap: ReferenceableMap): String = {
    val identifier = "x"
    val node = AnyNode()
    val fakeMap: ReferenceableMap = Map(node -> identifier)
    val conditionString = where(node).toQuery(fakeMap)
    s"${predicate.toQuery} ($identifier in ${projection.toQuery(referenceableMap)} where $conditionString)"
  }

  def referenceables: Set[Referenceable] =
    projection.referenceables
}

case class Expression(string: String, references: ReferenceType*) extends Condition {
  def toQuery(referenceableMap: ReferenceableMap): String = {
    val questionMarksCount = (string filter (_ == '?')).size
    if (questionMarksCount != references.size)
      throw new MismatchedInterpolatedStringWithReferences(string, questionMarksCount, references.size)
    else {
      val expression = references.foldLeft(string) { (acc, reference) =>
        // avoiding replaceFirst because it has special handling of escape characters:
        // http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceFirst%28java.lang.String,%20java.lang.String%29
        val pieces = acc.split("[?]")
        Seq(pieces.lift(0), Some(reference.toQuery(referenceableMap)), pieces.lift(1)).flatten mkString ""
      }
      s"($expression)"
    }
  }

  def referenceables: Set[Referenceable] =
    (references flatMap (_.getReferenceable)).toSet
}
