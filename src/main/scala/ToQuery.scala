package com.originate.scalypher

import types._

trait ToQuery {
  def toQuery: String
}

trait ToQueryWithIdentifiers {
  def toQuery(referenceableMap: ReferenceableMap): String
}
