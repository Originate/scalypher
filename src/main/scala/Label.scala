package com.originate.scalypher

import scala.language.implicitConversions

case class Label(name: String) {
  def toQuery: String =
    s":$name"
}

object Label {
  implicit def stringToLabel(string: String): Label =
    Label(string)
}
