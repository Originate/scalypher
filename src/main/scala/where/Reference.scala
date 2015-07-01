package com.originate.scalypher.where

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.CypherExpressible
import com.originate.scalypher.PropertyAssignment
import com.originate.scalypher.PropertyName
import com.originate.scalypher.RemovePropertyAssignment
import com.originate.scalypher.SetProperty
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException

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

  protected def toQueryWithProperty(
    identifiableMap: IdentifiableMap,
    identifiable: Identifiable,
    property: Option[PropertyName] = None
  ): String = {
    val identifier = identifiableMap.get(identifiable) getOrElse (throw new IdentifierDoesntExistException())
    val propertyString = property map (p => s".${p.name}") getOrElse ""
    s"$identifier$propertyString"
  }

}

object Reference {
  implicit def toActionReference(reference: ObjectReference): ActionItem =
    ActionReference(reference)

  implicit def toActionReference(reference: ReferenceWithProperty): ActionItem =
    ActionReference(reference)

  implicit def objectReferencesToActionReferences(references: Seq[ObjectReference]): Seq[ActionItem] =
    references map (ActionReference.apply(_))

  implicit def referencesWithPropertiesToActionReferences(
    references: Seq[ReferenceWithProperty]
  ): Seq[ActionItem] =
    references map (ActionReference.apply(_))

  implicit def toValueReference[V : CypherExpressible](value: V): ValueReference[V] =
    ValueReference[V](value)

  implicit def seqToValueReference[V : CypherExpressible](values: Seq[V]): SeqValueReference[V] =
    SeqValueReference[V](values)
}

sealed trait ReferenceableReference extends Reference

case class ObjectReference(identifiable: Referenceable) extends ReferenceableReference {
  def property(property: String): ReferenceWithProperty =
    ReferenceWithProperty(identifiable, PropertyName(property))

  def toQuery(identifiableMap: IdentifiableMap): String =
    toQueryWithProperty(identifiableMap, identifiable, None)

  def getReferenceable = Some(identifiable)
}

case class ReferenceWithProperty(
  identifiable: Referenceable,
  property: PropertyName
) extends ReferenceableReference {
  def :=[T](reference: ValueReference[T]): PropertyAssignment[T] =
    assign(reference)

  def assign[T](reference: ValueReference[T]): PropertyAssignment[T] =
    PropertyAssignment(this, reference)

  def assignNull: RemovePropertyAssignment =
    RemovePropertyAssignment(this)

  def toQuery(identifiableMap: IdentifiableMap): String =
    toQueryWithProperty(identifiableMap, identifiable, Some(property))

  def set[T : CypherExpressible](value: T): SetProperty =
    SetProperty(this, value)

  def getReferenceable = Some(identifiable)
}

case class ValueReference[V](value: V)(implicit serializer: CypherExpressible[V]) extends Reference {
  def toQuery(identifiableMap: IdentifiableMap): String =
    serializer.toQuery(value)

  def getReferenceable = None
}

case class SeqValueReference[V](values: Seq[V])(implicit serializer: CypherExpressible[V]) extends Reference {
  def toQuery(identifiableMap: IdentifiableMap): String =
    "[" + (values map serializer.toQuery mkString ", ") + "]"

  def getReferenceable = None
}
