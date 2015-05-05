package com.originate.scalypher

trait Serializable[V] {
  def toQuery(value: V): String

  protected def safeWrapString(string: String): String =
    wrapString(string.replaceAll("\"", """\\\""""))

  protected def wrapString(string: String): String =
    "\"" + string + "\""
}

object Serializable {
  implicit object SerializableString extends Serializable[String] {
    def toQuery(string: String): String =
      safeWrapString(string)
  }

  implicit object SerializableInt extends Serializable[Int] {
    def toQuery(int: Int): String =
      int.toString
  }

  implicit object SerializableDouble extends Serializable[Double] {
    def toQuery(double: Double): String =
      double.toString
  }
}
