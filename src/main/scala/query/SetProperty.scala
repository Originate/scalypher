package com.originate.scalypher

import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.ReferenceWithProperty
import com.originate.scalypher.types.Identifiable

class SetProperty private (reference: ReferenceWithProperty, serializedValue: String) {
  def getReferenceable: Option[Identifiable] = reference.getReferenceable

  def toQuery(identifiableMap: ReferenceableMap): String =
    s"${reference.toQuery(identifiableMap)} = $serializedValue"
}

object SetProperty {
  def apply[T](reference: ReferenceWithProperty, value: T)(implicit serializer: CypherExpressible[T]): SetProperty = {
    new SetProperty(reference, serializer.toQuery(value))
  }

  private[scalypher] def withSerializedValue(reference: ReferenceWithProperty, serializedValue: String): SetProperty =
    new SetProperty(reference, serializedValue)
}
