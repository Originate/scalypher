package com.originate.scalypher.path

import com.originate.scalypher.types.ReferenceableMap

sealed case class ArrowType(firstPart: String, secondPart: String) {
  def toQuery(referenceableMap: ReferenceableMap, relationship: Option[Relationship] = None): String =
    firstPart + (relationship map (_.toQuery(referenceableMap)) getOrElse "") + secondPart
}

object RightArrow extends ArrowType("-", "->")
object LeftArrow extends ArrowType("<-", "-")
object DirectionlessArrow extends ArrowType("-", "-")
