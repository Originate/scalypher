package com.originate.scalypher.action

import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.PropertyReference
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.ReferenceType

sealed trait Action {
  def referenceables: Set[Referenceable]
  def toQuery(referenceableMap: ReferenceableMap): String
}

abstract class ReferenceListAction(keyword: String) extends Action {
  def reference: ReferenceType

  def rest: Seq[ReferenceType]

  private val references: Set[ReferenceType] = (reference +: rest).toSet

  def referenceables: Set[Referenceable] =
    references flatMap (_.getReferenceable)

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"$keyword " + (references map (_.toQuery(referenceableMap)) mkString ", ")
}

case class ReturnReference(reference: ReferenceType, rest: ReferenceType*) extends ReferenceListAction("RETURN")
case class ReturnDistinct(reference: ReferenceType, rest: ReferenceType*) extends ReferenceListAction("RETURN DISTINCT")
case class Delete(reference: ReferenceType, rest: ReferenceType*) extends ReferenceListAction("DELETE")

case object ReturnAll extends Action {
  def referenceables: Set[Referenceable] = Set()

  def toQuery(referenceableMap: ReferenceableMap): String =
    "RETURN *"
}

