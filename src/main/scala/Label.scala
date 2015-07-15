package com.originate.scalypher

import com.originate.scalypher.util.Exceptions.CharacterNotAllowedInLabel

import scala.language.implicitConversions

case class Label(name: String) {
  def toQuery: String =
    s":$name"

  def escapedName: String =
    if (name contains "`") throw new CharacterNotAllowedInLabel('`', name)
    else if (name contains " ") s"`$name`"
    else name
}

object Label {
  implicit def stringToLabel(string: String): Label =
    Label(string)
}
