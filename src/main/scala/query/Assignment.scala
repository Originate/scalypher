package com.originate.scalypher

import com.originate.scalypher.path.Node
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.ReferenceWithProperty
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.ValueReference

sealed trait Assignment extends ToQueryWithIdentifiers {
  def referenceables: Set[Referenceable]
}

case class RemovePropertyAssignment(reference: ReferenceWithProperty) extends Assignment {
  def referenceables: Set[Referenceable] =
    reference.getReferenceable.toSet

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"${reference.toQuery(identifiableMap)} = NULL"
}

case class MergePropertiesAssignment(
  reference: ObjectReference,
  properties: Seq[Property]
) extends Assignment {
  def referenceables: Set[Referenceable] =
    reference.getReferenceable.toSet

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"${reference.toQuery(identifiableMap)} += ${Property.toQuery(properties)}"
}

case class OverwriteAssignment(lhs: ObjectReference, rhs: ObjectReference) extends Assignment {
  def referenceables: Set[Referenceable] =
    Set(lhs.getReferenceable, rhs.getReferenceable).flatten

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"${lhs.toQuery(identifiableMap)} = ${rhs.toQuery(identifiableMap)}"
}

case class PropertyAssignment[T](lhs: ReferenceWithProperty, rhs: ValueReference[T]) extends Assignment {
  def referenceables: Set[Referenceable] =
    lhs.getReferenceable.toSet

  def toQuery(identifiableMap: IdentifiableMap): String =
    s"${lhs.toQuery(identifiableMap)} = ${rhs.toQuery(identifiableMap)}"
}
