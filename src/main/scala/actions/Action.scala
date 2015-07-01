package com.originate.scalypher.action

import com.originate.scalypher.PropertyName
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
  def reference: ActionItem

  def rest: Seq[ActionItem]

  private val references: Set[ActionItem] = (reference +: rest).toSet

  def identifiables: Set[Identifiable] =
    references flatMap (_.getIdentifiable)

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"$keyword " + (references map (_.toQuery(identifiableMap)) mkString ", ")

  def returnColumns(identifiableMap: IdentifiableMap): Set[String] =
    references.map(_.toColumn(identifiableMap)).toSet
}

case class Delete(reference: ActionItem, rest: ActionItem*) extends ReferenceListAction("DELETE")

case class ReturnReference(reference: ActionItem, rest: ActionItem*) extends ReferenceListAction("RETURN") with ReturnAction

case class ReturnDistinct(reference: ActionItem, rest: ActionItem*) extends ReferenceListAction("RETURN DISTINCT") with ReturnAction

case object ReturnAll extends ReturnAction {
  def identifiables: Set[Identifiable] = Set()

  def toQuery(identifiableMap: IdentifiableMap): String =
    "RETURN *"
}
