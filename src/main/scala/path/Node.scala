package com.originate.scalypher.path

import com.originate.scalypher.AddableProperties
import com.originate.scalypher.Label
import com.originate.scalypher.OverwriteAssignment
import com.originate.scalypher.path.Path.getIdentifierOrEmptyString
import com.originate.scalypher.Property
import com.originate.scalypher.PropertyName
import com.originate.scalypher.Query
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.where.HasNoRelationships
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.ReferenceWithProperty

import scala.language.implicitConversions

sealed trait Node
    extends ToQueryWithIdentifiers
    with Referenceable
    with AddableProperties {

  def property(propertyName: String): ReferenceWithProperty =
    ReferenceWithProperty(this, PropertyName(propertyName))

  def :=(node: Node): OverwriteAssignment =
    assign(node)

  def assign(node: Node): OverwriteAssignment =
    OverwriteAssignment(ObjectReference(this), ObjectReference(node))

  def hasNoRelationships(labels: Seq[Label] = Seq.empty): HasNoRelationships =
    HasNoRelationships(this, labels)

}

object Node {
  implicit def toPath(node: Node): Path =
    Path(node)
}

class AnyNode extends Node {
  def toQuery(identifiableMap: IdentifiableMap): String = {
    val identifier = getIdentifierOrEmptyString(identifiableMap, this)
    s"($identifier)"
  }
}

object AnyNode {
  def apply(): AnyNode =
    new AnyNode
}

class CypherNode(
  val labels: Seq[Label] = Seq.empty,
  val properties: Seq[Property] = Seq.empty
) extends Node {
  def toQuery(identifiableMap: IdentifiableMap): String = {
    val identifier = getIdentifierOrEmptyString(identifiableMap, this)
    val labelsQuery = labels map (_.toQuery) mkString ""
    val propertiesQuery = Property.toQuery(properties)

    s"($identifier$labelsQuery$propertiesQuery)"
  }
}

object CypherNode {

  def apply(label: Label): CypherNode =
    new CypherNode(labels = Seq(label))

  def apply(property: Property): CypherNode =
    new CypherNode(properties = Seq(property))

  def apply(label: Label, properties: Property*): CypherNode =
    new CypherNode(Seq(label), properties)

  def apply(labels: Seq[String], properties: Seq[Property]): CypherNode =
    new CypherNode(labels map (Label(_)), properties)

}
