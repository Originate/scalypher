package com.originate.scalypher

import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.Reference
import com.originate.scalypher.types.Referenceable

class SetProperty private (reference: Reference, serializedValue: String) {
  def getReferenceable: Option[Referenceable] = reference.getReferenceable

  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${reference.toQuery(referenceableMap)} = $serializedValue"
}

object SetProperty {
  def apply[T](reference: Reference, value: T)(implicit serializer: CypherExpressible[T]) {
    new SetProperty(reference, serializer.toQuery(value))
  }
}
