package com.originate.scalypher.path

import com.originate.scalypher.AddableProperties
import com.originate.scalypher.Label
import com.originate.scalypher.path.Path.getIdentifierOrEmptyString
import com.originate.scalypher.Property
import com.originate.scalypher.PropertyName
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.where.ReferenceWithProperty

sealed trait Relationship
    extends ToQueryWithIdentifiers
    with Referenceable
    with AddableProperties {

  def --(node: Node): PathPiece =
    PathPiece(DirectionlessArrow, node, this)

  def property(propertyName: String): ReferenceWithProperty =
    ReferenceWithProperty(this, PropertyName(propertyName))

  protected def makeRelationshipString(
    identifiableMap: IdentifiableMap,
    kinds: Seq[Label],
    rest: String = ""
  ): String =
    s"[${getIdentifierOrEmptyString(identifiableMap, this)}${Relationship.kindsToQuery(kinds)}$rest]"

}

object Relationship {

  def kindsToQuery(kinds: Seq[Label]): String =
    if (kinds.isEmpty) ""
    else ":" + (kinds map (_.escapedName) mkString "|")

}

class CreateRelationship(val kind: Label, val properties: Seq[Property]) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, Seq(kind), Property.toQuery(properties))
}

object CreateRelationship {
  def apply(kind: Label, properties: Seq[Property] = Seq.empty): CreateRelationship =
    new CreateRelationship(kind, properties)
}

class AnyRelationship() extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, Seq.empty)
}

object AnyRelationship {
  def apply(): AnyRelationship =
    new AnyRelationship()
}

class KindRelationship(val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds)
}

object KindRelationship {
  def apply(kinds: Label*): KindRelationship =
    new KindRelationship(kinds: _*)
}

class DistanceRelationship(val length: Int, val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$length")
}

object DistanceRelationship {
  def apply(length: Int, kinds: Label*): DistanceRelationship =
    new DistanceRelationship(length, kinds: _*)
}

class MaxDistanceRelationship(val length: Int, val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*..$length")
}

object MaxDistanceRelationship {
  def apply(length: Int, kinds: Label*): MaxDistanceRelationship =
    new MaxDistanceRelationship(length, kinds: _*)
}

class MinDistanceRelationship(val length: Int, val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$length..")
}

object MinDistanceRelationship {
  def apply(length: Int, kinds: Label*): MinDistanceRelationship =
    new MinDistanceRelationship(length, kinds: _*)
}

class RangeRelationship(val min: Int, max: Int, val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$min..$max")
}

object RangeRelationship {
  def apply(min: Int, max: Int, kinds: Label*): RangeRelationship =
    new RangeRelationship(min, max, kinds: _*)
}

class AnyLengthRelationship(val kinds: Label*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*")
}

object AnyLengthRelationship {
  def apply(kinds: Label*): AnyLengthRelationship =
    new AnyLengthRelationship(kinds: _*)
}
