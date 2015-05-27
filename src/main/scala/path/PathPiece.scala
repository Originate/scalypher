package com.originate.scalypher.path

import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap

case class PathPieces(startPiece: PathPiece, tail: Seq[PathPiece], relationship: Option[RelationshipType]) {
  def --(node: Node): PathPieces =
    copy(tail = tail :+ PathPiece(DirectionlessArrow, node, relationship), relationship = None)

  def -->(node: Node): PathPieces =
    copy(tail = tail :+ PathPiece(RightArrow, node, relationship), relationship = None)
}

object PathPieces {
  def apply(pathPiece: PathPiece, relationship: RelationshipType): PathPieces =
    PathPieces(pathPiece, Seq(), Some(relationship))
}

case class PathPiece(
  arrow: ArrowType,
  node: Node,
  relationship: Option[RelationshipType] = None
) {

  def replaceNode(oldNode: Node, newNode: Node): PathPiece =
    if (node == oldNode) copy(node = newNode)
    else this

  def replaceRelationship(oldRelationship: RelationshipType, newRelationship: RelationshipType): PathPiece =
    if (relationship exists (_ == oldRelationship)) copy(relationship = Some(newRelationship))
    else this

  def --(node: Node): PathPieces =
    PathPieces(this, Seq(PathPiece(DirectionlessArrow, node)), None)

  def --(relationship: RelationshipType): PathPieces =
    PathPieces(this, Seq(), Some(relationship))

  def -->(node: Node): PathPieces =
    PathPieces(this, Seq(PathPiece(RightArrow, node)), None)

  def toQuery(referenceableMap: ReferenceableMap): String =
    arrow.toQuery(referenceableMap, relationship) + node.toQuery(referenceableMap)

}

object PathPiece {
  def apply(
    arrow: ArrowType,
    node: Node,
    relationship: RelationshipType
  ): PathPiece =
    PathPiece(arrow, node, Some(relationship))
}
