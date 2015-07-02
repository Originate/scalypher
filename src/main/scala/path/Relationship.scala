package com.originate.scalypher.path

import com.originate.scalypher.AddableProperties
import com.originate.scalypher.path.Path.getIdentifierOrEmptyString
import com.originate.scalypher.Property
import com.originate.scalypher.PropertyName
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.util.Exceptions.CharacterNotAllowedInLabel
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
    kinds: Seq[String],
    rest: String = ""
  ): String =
    s"[${getIdentifierOrEmptyString(identifiableMap, this)}${kindsToQuery(kinds)}$rest]"

  private def kindsToQuery(kinds: Seq[String]): String =
    if (kinds.isEmpty) ""
    else ":" + (kinds map escape mkString "|")

  private def escape(kind: String): String =
    if (kind contains '`') throw new CharacterNotAllowedInLabel('`', kind)
    else if (kind contains ' ') s"`$kind`"
    else kind

}

class CreateRelationship(val kind: String, val properties: Seq[Property]) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, Seq(kind), Property.toQuery(properties))
}

object CreateRelationship {
  def apply(kind: String, properties: Seq[Property] = Seq.empty): CreateRelationship =
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

class KindRelationship(val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds)
}

object KindRelationship {
  def apply(kinds: String*): KindRelationship =
    new KindRelationship(kinds: _*)
}

class DistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$length")
}

object DistanceRelationship {
  def apply(length: Int, kinds: String*): DistanceRelationship =
    new DistanceRelationship(length, kinds: _*)
}

class MaxDistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*..$length")
}

object MaxDistanceRelationship {
  def apply(length: Int, kinds: String*): MaxDistanceRelationship =
    new MaxDistanceRelationship(length, kinds: _*)
}

class MinDistanceRelationship(val length: Int, val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$length..")
}

object MinDistanceRelationship {
  def apply(length: Int, kinds: String*): MinDistanceRelationship =
    new MinDistanceRelationship(length, kinds: _*)
}

class RangeRelationship(val min: Int, max: Int, val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*$min..$max")
}

object RangeRelationship {
  def apply(min: Int, max: Int, kinds: String*): RangeRelationship =
    new RangeRelationship(min, max, kinds: _*)
}

class AnyLengthRelationship(val kinds: String*) extends Relationship {
  def toQuery(identifiableMap: IdentifiableMap): String =
    makeRelationshipString(identifiableMap, kinds, s"*")
}

object AnyLengthRelationship {
  def apply(kinds: String*): AnyLengthRelationship =
    new AnyLengthRelationship(kinds: _*)
}
