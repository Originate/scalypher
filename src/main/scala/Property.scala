package com.originate.scalypher

class Property private (key: String, serializedValue: String) {
  def toQuery: String =
    s"""$key:$serializedValue"""
}

object Property {
  def apply[T](tuple: (String, T))(implicit serializer: Serializable[T]): Property =
    new Property(tuple._1, serializer.toQuery(tuple._2))

  def apply[T](key: String, value: T)(implicit serializer: Serializable[T]): Property =
    new Property(key, serializer.toQuery(value))
}
