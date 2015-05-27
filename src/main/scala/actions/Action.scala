package com.originate.scalypher.action

import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.PropertyName
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.Reference


sealed trait Action {
  def referenceables: Set[Referenceable]
  def toQuery(referenceableMap: ReferenceableMap): String
}
sealed trait ReturnAction extends Action

abstract class ReferenceListAction(keyword: String) extends Action {
  def reference: Reference

  def rest: Seq[Reference]

  private val references: Set[Reference] = (reference +: rest).toSet

  def referenceables: Set[Referenceable] =
    references flatMap (_.getReferenceable)

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"$keyword " + (references map (_.toQuery(referenceableMap)) mkString ", ")
}

case class Delete(reference: Reference, rest: Reference*) extends ReferenceListAction("DELETE")

case class ReturnReference(reference: Reference, rest: Reference*) extends ReferenceListAction("RETURN") with ReturnAction

case class ReturnDistinct(reference: Reference, rest: Reference*) extends ReferenceListAction("RETURN DISTINCT") with ReturnAction

case object ReturnAll extends ReturnAction {
  def referenceables: Set[Referenceable] = Set()

  def toQuery(referenceableMap: ReferenceableMap): String =
    "RETURN *"
}

