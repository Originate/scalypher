package com.originate.scalypher

case class Label(name: String) {
  def toQuery: String =
    s":$name"
}
