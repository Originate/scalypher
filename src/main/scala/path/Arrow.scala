package com.scalypher.path

import com.scalypher.types.ReferenceableMap

sealed trait ArrowType {
  def firstPart: String
  def secondPart: String

  def toQuery(referenceableMap: ReferenceableMap, relationship: Option[RelationshipType] = None): String =
    firstPart + (relationship map (_.toQuery(referenceableMap)) getOrElse "") + secondPart
}

abstract class ConstantArrow(val firstPart: String, val secondPart: String)

case object RightArrow extends ConstantArrow("-", "->") with ArrowType
case object LeftArrow extends ConstantArrow("<-", "-") with ArrowType
case object DirectionlessArrow extends ConstantArrow("-", "-") with ArrowType
