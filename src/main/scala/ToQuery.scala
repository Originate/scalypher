package com.originate.scalypher

import com.originate.scalypher.types._

trait ToQuery {
  def toQuery: String
}

trait ToQueryWithIdentifiers {
  def toQuery(identifiableMap: IdentifiableMap): String
}
