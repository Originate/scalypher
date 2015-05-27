package com.originate.scalypher.where

import com.originate.scalypher.Query.toQueryWithProperty
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.PropertyReference
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.CypherExpressible
import scala.language.implicitConversions

sealed trait ReferenceType extends ToQueryWithIdentifiers {
  def getReferenceable: Option[Referenceable] = None

  def ===(reference: ReferenceType): Condition =
    Comparison(this, Equal, reference)

  def <>(reference: ReferenceType): Condition =
    Comparison(this, NotEqual, reference)

  def <(reference: ReferenceType): Condition =
    Comparison(this, LT, reference)

  def >(reference: ReferenceType): Condition =
    Comparison(this, GT, reference)

  def <=(reference: ReferenceType): Condition =
    Comparison(this, LTE, reference)

  def >=(reference: ReferenceType): Condition =
    Comparison(this, GTE, reference)

}

object ReferenceType {
  implicit def toValueReference[V : CypherExpressible](value: V): ValueReference[V] =
    ValueReference[V](value)
}

case class Reference(referenceable: Referenceable) extends ReferenceType {
  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, None)

  override def getReferenceable = Some(referenceable)
}

case class ReferenceWithProperty(referenceable: Referenceable, property: PropertyReference) extends ReferenceType {
  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, Some(property))

  override def getReferenceable = Some(referenceable)
}

case class ValueReference[V](value: V)(implicit serializer: CypherExpressible[V]) extends ReferenceType {
  def toQuery(referenceableMap: ReferenceableMap): String =
    serializer.toQuery(value)
}
