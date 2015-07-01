package com.originate.scalypher.action

import com.originate.scalypher.PropertyName
import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.Reference

sealed trait Action {
  def identifiables: Set[Identifiable]
  def toQuery(identifiableMap: IdentifiableMap): String
}
sealed trait ReturnAction extends Action

abstract class ReferenceListAction(keyword: String) extends Action {
  def reference: ActionReference

  def rest: Seq[ActionReference]

  private val references: Set[ActionReference] = (reference +: rest).toSet

  def identifiables: Set[Identifiable] =
    references flatMap (_.getReferenceable)

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"$keyword " + (references map (_.toQuery(identifiableMap)) mkString ", ")

  def returnColumns(identifiableMap: IdentifiableMap): Set[String] =
    references.map(_.toColumn(identifiableMap)).toSet
}

case class Delete(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("DELETE")

case class ReturnReference(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN") with ReturnAction

case class ReturnDistinct(reference: ActionReference, rest: ActionReference*) extends ReferenceListAction("RETURN DISTINCT") with ReturnAction

case object ReturnAll extends ReturnAction {
  def identifiables: Set[Identifiable] = Set()

  def toQuery(identifiableMap: IdentifiableMap): String =
    "RETURN *"
}
