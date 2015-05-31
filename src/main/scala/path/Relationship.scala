package com.originate.scalypher.path

import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.path.Path.getIdentifierOrEmptyString
import com.originate.scalypher.where.ReferenceWithProperty
import com.originate.scalypher.PropertyName
import com.originate.scalypher.Property

sealed trait Relationship extends ToQueryWithIdentifiers with Referenceable {
  def --(node: Node): PathPiece =
    PathPiece(DirectionlessArrow, node, this)

  def property(propertyName: String): ReferenceWithProperty =
    ReferenceWithProperty(this, PropertyName(propertyName))

  protected def makeRelationshipString(
    referenceableMap: ReferenceableMap,
    kinds: Seq[String],
    rest: String = ""
  ): String =
    s"[${getIdentifierOrEmptyString(referenceableMap, this)}${kindsToQuery(kinds)}$rest]"

  private def kindsToQuery(kinds: Seq[String]): String =
    if (kinds.isEmpty) ""
    else ":" + (kinds mkString "|")
}

class CreateRelationship(val kind: String, val properties: Seq[Property]) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, Seq(kind), Property.toQuery(properties))
}

object CreateRelationship {
  def apply(kind: String, properties: Seq[Property] = Seq.empty): CreateRelationship =
    new CreateRelationship(kind, properties)
}

class AnyRelationship() extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, Seq.empty)
}

object AnyRelationship {
  def apply(): AnyRelationship =
    new AnyRelationship()
}

class KindRelationship(val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds)
}

object KindRelationship {
  def apply(kinds: String*): KindRelationship =
    new KindRelationship(kinds: _*)
}

class DistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds, s"*$length")
}

object DistanceRelationship {
  def apply(length: Int, kinds: String*): DistanceRelationship =
    new DistanceRelationship(length, kinds: _*)
}

class MaxDistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds, s"*..$length")
}

object MaxDistanceRelationship {
  def apply(length: Int, kinds: String*): MaxDistanceRelationship =
    new MaxDistanceRelationship(length, kinds: _*)
}

class MinDistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds, s"*$length..")
}

object MinDistanceRelationship {
  def apply(length: Int, kinds: String*): MinDistanceRelationship =
    new MinDistanceRelationship(length, kinds: _*)
}

class RangeRelationship(val min: Int, max: Int, val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds, s"*$min..$max")
}

object RangeRelationship {
  def apply(min: Int, max: Int, kinds: String*): RangeRelationship =
    new RangeRelationship(min, max, kinds: _*)
}

class AnyLengthRelationship(val kinds: String*) extends Relationship {
  def toQuery(referenceableMap: ReferenceableMap): String =
    makeRelationshipString(referenceableMap, kinds, s"*")
}

object AnyLengthRelationship {
  def apply(kinds: String*): AnyLengthRelationship =
    new AnyLengthRelationship(kinds: _*)
}

case class DanglingRelationship(path: Path, relationship: Relationship) {
  def --(node: Node): Path =
    path --(relationship, node)

  def -->(node: Node): Path =
    path -->(relationship, node)
}
