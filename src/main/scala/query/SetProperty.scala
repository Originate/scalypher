package com.originate.scalypher

import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.ReferenceWithProperty
import com.originate.scalypher.types.Referenceable

class SetProperty private (reference: ReferenceWithProperty, serializedValue: String) {
  def getReferenceable: Option[Referenceable] = reference.getReferenceable

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${reference.toQuery(referenceableMap)} = $serializedValue"
}

object SetProperty {
  def apply[T](reference: ReferenceWithProperty, value: T)(implicit serializer: CypherExpressible[T]): SetProperty = {
    new SetProperty(reference, serializer.toQuery(value))
  }

  private[scalypher] def withSerializedValue(reference: ReferenceWithProperty, serializedValue: String): SetProperty =
    new SetProperty(reference, serializedValue)
}
