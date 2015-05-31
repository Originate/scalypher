package com.originate.scalypher.where

import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.PropertyName
import com.originate.scalypher.SetProperty
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.CypherExpressible
import scala.language.implicitConversions

sealed trait Reference extends ToQueryWithIdentifiers {
  def getReferenceable: Option[Referenceable] = None

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

}

object Reference {
  implicit def toValueReference[V : CypherExpressible](value: V): ValueReference[V] =
    ValueReference[V](value)
}

case class ObjectReference(referenceable: Referenceable) extends Reference {
  def property(property: String): ReferenceWithProperty =
    ReferenceWithProperty(referenceable, PropertyName(property))

  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, None)

  override def getReferenceable = Some(referenceable)
}

case class ReferenceWithProperty(referenceable: Referenceable, property: PropertyName) extends Reference {
  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, Some(property))

  def set[T : CypherExpressible](value: T): SetProperty =
    SetProperty(this, value)

  override def getReferenceable = Some(referenceable)
}

case class ValueReference[V](value: V)(implicit serializer: CypherExpressible[V]) extends Reference {
  def toQuery(referenceableMap: ReferenceableMap): String =
    serializer.toQuery(value)
}
