package com.originate.scalypher

trait CypherExpressible[V] {
  def toQuery(value: V): String

  protected def safeWrapString(string: String): String =
    wrapString(string.replaceAll("\"", """\\\""""))

  protected def wrapString(string: String): String =
    "\"" + string + "\""
}

object CypherExpressible {

  implicit object CypherExpressibleString extends CypherExpressible[String] {
    def toQuery(string: String): String =
      safeWrapString(string)
  }

  implicit object CypherExpressibleInt extends CypherExpressible[Int] {
    def toQuery(int: Int): String =
      int.toString
  }

  implicit object CypherExpressibleDouble extends CypherExpressible[Double] {
    def toQuery(double: Double): String =
      double.toString
  }

  implicit object CypherExpressibleBoolean extends CypherExpressible[Boolean] {
    def toQuery(boolean: Boolean): String =
      if (boolean) "TRUE" else "FALSE"
  }

  implicit def optionExpressible[T](implicit serializer: CypherExpressible[T]): CypherExpressible[Option[T]] =
    new CypherExpressible[Option[T]] {
      def toQuery(valueOption: Option[T]): String =
        valueOption map serializer.toQuery getOrElse "NULL"
    }

}
