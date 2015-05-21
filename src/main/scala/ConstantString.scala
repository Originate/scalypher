package com.originate.scalypher

abstract class ConstantString(stringified: String) {
  def toQuery: String = stringified
}
