package com.originate.scalypher

import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.types.Referenceable

class Property private (key: String, serializedValue: String) {
  def toQuery: String =
    s"""$key:$serializedValue"""

  def toSetProperty(referenceable: Referenceable): SetProperty =
    toSetProperty(ObjectReference(referenceable))

  def toSetProperty(reference: ObjectReference): SetProperty =
    SetProperty.withSerializedValue(reference.property(key), serializedValue)
}

object Property {
  def apply[T](tuple: (String, T))(implicit serializer: CypherExpressible[T]): Property =
    new Property(tuple._1, serializer.toQuery(tuple._2))

  def apply[T](key: String, value: T)(implicit serializer: CypherExpressible[T]): Property =
    new Property(key, serializer.toQuery(value))

  def toQuery(properties: Seq[Property]): String =
    if (properties.isEmpty) ""
    else "{" + (properties map (_.toQuery) mkString ",") + "}"

}
