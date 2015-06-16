package com.originate.scalypher.action

import com.originate.scalypher.PropertyName
import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.Reference

sealed trait Action {
  def referenceables: Set[Referenceable]
  def toQuery(referenceableMap: ReferenceableMap): String
}
sealed trait ReturnAction extends Action

abstract class ReferenceListAction(keyword: String) extends Action {
  def reference: ActionReference

  def rest: Seq[ActionReference]

  private val references: Set[ActionReference] = (reference +: rest).toSet

  def referenceables: Set[Referenceable] =
    references flatMap (_.reference.getReferenceable)

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"$keyword " + (references map (_.toQuery(referenceableMap)) mkString ", ")

  def returnColumns(referenceableMap: ReferenceableMap): Set[String] =
    references.map(_.toColumn(referenceableMap)).toSet
}

case class Delete(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("DELETE")

case class ReturnReference(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN") with ReturnAction

case class ReturnDistinct(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN DISTINCT") with ReturnAction

case object ReturnAll extends ReturnAction {
  def referenceables: Set[Referenceable] = Set()

  def toQuery(referenceableMap: ReferenceableMap): String =
    "RETURN *"
}

case class ActionReference(reference: Reference, as: Option[String] = None) {
  def as(name: String): ActionReference =
    copy(as = Some(name))

  def toQuery(referenceableMap: ReferenceableMap): String = {
    val asString = as map { name =>
      if (referenceableMap.values.toSet contains name) throw new IdentifierAliasCollisionException(name)
      s"AS $name"
    }
    Seq(Some(reference.toQuery(referenceableMap)), asString).flatten mkString " "
  }

  def toColumn(referenceableMap: ReferenceableMap): String =
    as match {
      case Some(column) => column
      case _ => reference.toQuery(referenceableMap)
    }

}
