package com.originate.scalypher

import com.originate.scalypher.path.Node
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.ReferenceWithProperty
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.ValueReference

sealed trait Assignment extends ToQueryWithIdentifiers {
  def referenceables: Set[Identifiable]
}

case class RemovePropertyAssignment(reference: ReferenceWithProperty) extends Assignment {
  def referenceables: Set[Identifiable] =
    reference.getReferenceable.toSet

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${reference.toQuery(referenceableMap)} = NULL"
}

case class MergePropertiesAssignment(
  reference: ObjectReference,
  properties: Seq[Property]
) extends Assignment {
  def referenceables: Set[Identifiable] =
    reference.getReferenceable.toSet

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${reference.toQuery(referenceableMap)} += ${Property.toQuery(properties)}"
}

case class OverwriteAssignment(lhs: ObjectReference, rhs: ObjectReference) extends Assignment {
  def referenceables: Set[Identifiable] =
    Set(lhs.getReferenceable, rhs.getReferenceable).flatten

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${lhs.toQuery(referenceableMap)} = ${rhs.toQuery(referenceableMap)}"
}

case class PropertyAssignment[T](lhs: ReferenceWithProperty, rhs: ValueReference[T]) extends Assignment {
  def referenceables: Set[Identifiable] =
    lhs.getReferenceable.toSet

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${lhs.toQuery(referenceableMap)} = ${rhs.toQuery(referenceableMap)}"
}
