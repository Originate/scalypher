package com.scalypher.where

import com.scalypher.Query.toQueryWithProperty
import com.scalypher.types.Referenceable
import com.scalypher.types.ReferenceableMap
import com.scalypher.PropertyReference
import com.scalypher.ToQueryWithIdentifiers
import com.scalypher.Serializable
import scala.language.implicitConversions

sealed trait ReferenceType extends ToQueryWithIdentifiers {
  def getReferenceable: Option[Referenceable] = None

  def ==(reference: ReferenceType): Condition =
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
  implicit def toValueReference[V : Serializable](value: V): ValueReference[V] =
    ValueReference[V](value)
}

case class Reference(referenceable: Referenceable, property: Option[PropertyReference] = None) extends ReferenceType {
  def toQuery(referenceableMap: ReferenceableMap): String =
    toQueryWithProperty(referenceableMap, referenceable, property)

  override def getReferenceable = Some(referenceable)
}

object Reference {
  def apply(referenceable: Referenceable, property: PropertyReference): Reference =
    Reference(referenceable, Some(property))
}

case class ValueReference[V : Serializable](value: V)(implicit serializer: Serializable[V]) extends ReferenceType {
  def toQuery(referenceableMap: ReferenceableMap): String =
    serializer.toQuery(value)
}
