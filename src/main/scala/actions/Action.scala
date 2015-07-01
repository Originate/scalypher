package com.originate.scalypher.action

import com.originate.scalypher.PropertyName
import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.Reference

sealed trait Action {
  def identifiables: Set[Identifiable]
  def toQuery(identifiableMap: ReferenceableMap): String
}
sealed trait ReturnAction extends Action

abstract class ReferenceListAction(keyword: String) extends Action {
  def reference: ActionReference

  def rest: Seq[ActionReference]

  private val references: Set[ActionReference] = (reference +: rest).toSet

  def identifiables: Set[Identifiable] =
    references flatMap (_.getReferenceable)

  def toQuery(identifiableMap: ReferenceableMap): String =
    s"$keyword " + (references map (_.toQuery(identifiableMap)) mkString ", ")

  def returnColumns(identifiableMap: ReferenceableMap): Set[String] =
    references.map(_.toColumn(identifiableMap)).toSet
}

case class Delete(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("DELETE")

case class ReturnReference(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN") with ReturnAction

case class ReturnDistinct(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN DISTINCT") with ReturnAction

case object ReturnAll extends ReturnAction {
  def identifiables: Set[Identifiable] = Set()

  def toQuery(identifiableMap: ReferenceableMap): String =
    "RETURN *"
}
