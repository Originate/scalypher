package com.originate.scalypher.where

import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.PropertyAssignment
import com.originate.scalypher.RemovePropertyAssignment
import com.originate.scalypher.PropertyName
import com.originate.scalypher.SetProperty
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.CypherExpressible
import scala.language.implicitConversions

sealed trait Reference extends ToQueryWithIdentifiers {
  def getReferenceable: Option[Referenceable]

  def ===(reference: Reference): Condition =
    Comparison(this, Equal, reference)

  def <>(reference: Reference): Condition =
    Comparison(this, NotEqual, reference)

  def <(reference: Reference): Condition =
    Comparison(this, LT, reference)

  def >(reference: Reference): Condition =
    Comparison(this, GT, reference)

  def <=(reference: Reference): Condition =
    Comparison(this, LTE, reference)

  def >=(reference: Reference): Condition =
    Comparison(this, GTE, reference)

  def in[V](reference: SeqValueReference[V]): Condition =
    Comparison(this, IN, reference)

}

object Reference {
  implicit def toActionReference(reference: Reference): ActionReference =
    ActionReference(reference)

  implicit def toActionReferenceSeq(references: Seq[Reference]): Seq[ActionReference] =
    references map (ActionReference(_))

  implicit def toValueReference[V : CypherExpressible](value: V): ValueReference[V] =
    ValueReference[V](value)

  implicit def seqToValueReference[V : CypherExpressible](values: Seq[V]): SeqValueReference[V] =
    SeqValueReference[V](values)
}

case class ObjectReference(referenceable: Referenceable) extends Reference {
  def property(property: String): ReferenceWithProperty =
    ReferenceWithProperty(referenceable, PropertyName(property))

  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, None)

  def getReferenceable = Some(referenceable)
}

case class ReferenceWithProperty(referenceable: Referenceable, property: PropertyName) extends Reference {
  def assign[T](reference: ValueReference[T]): PropertyAssignment[T] =
    PropertyAssignment(this, reference)

  def assignNull: RemovePropertyAssignment =
    RemovePropertyAssignment(this)

  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, Some(property))

  def set[T : CypherExpressible](value: T): SetProperty =
    SetProperty(this, value)

  def getReferenceable = Some(referenceable)
}

case class ValueReference[V](value: V)(implicit serializer: CypherExpressible[V]) extends Reference {
  def toQuery(referenceableMap: ReferenceableMap): String =
    serializer.toQuery(value)

  def getReferenceable = None
}

case class SeqValueReference[V](values: Seq[V])(implicit serializer: CypherExpressible[V]) extends Reference {
  def toQuery(referenceableMap: ReferenceableMap): String =
    "[" + (values map serializer.toQuery mkString ", ") + "]"

  def getReferenceable = None
}
