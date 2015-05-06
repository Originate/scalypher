package com.originate.scalypher

trait CypherExpressable[V] {
  def toQuery(value: V): String

  protected def safeWrapString(string: String): String =
    wrapString(string.replaceAll("\"", """\\\""""))

  protected def wrapString(string: String): String =
    "\"" + string + "\""
}

object CypherExpressable {
  implicit object CypherExpressableString extends CypherExpressable[String] {
    def toQuery(string: String): String =
      safeWrapString(string)
  }

  implicit object CypherExpressableInt extends CypherExpressable[Int] {
    def toQuery(int: Int): String =
      int.toString
  }

  implicit object CypherExpressableDouble extends CypherExpressable[Double] {
    def toQuery(double: Double): String =
      double.toString
  }
}
