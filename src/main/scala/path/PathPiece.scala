package com.originate.scalypher.path

import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap

case class PathPieces(startPiece: PathPiece, tail: Seq[PathPiece], relationship: Option[Relationship]) {
  def --(node: Node): PathPieces =
    copy(tail = tail :+ PathPiece(DirectionlessArrow, node, relationship), relationship = None)

  def -->(node: Node): PathPieces =
    copy(tail = tail :+ PathPiece(RightArrow, node, relationship), relationship = None)
}

object PathPieces {
  def apply(pathPiece: PathPiece, relationship: Relationship): PathPieces =
    PathPieces(pathPiece, Seq(), Some(relationship))
}

case class PathPiece(
  arrow: Arrow,
  node: Node,
  relationship: Option[Relationship] = None
) {

  def replaceNode(oldNode: Node, newNode: Node): PathPiece =
    if (node == oldNode) copy(node = newNode)
    else this

  def replaceRelationship(oldRelationship: Relationship, newRelationship: Relationship): PathPiece =
    if (relationship exists (_ == oldRelationship)) copy(relationship = Some(newRelationship))
    else this

  def --(node: Node): PathPieces =
    PathPieces(this, Seq(PathPiece(DirectionlessArrow, node)), None)

  def --(relationship: Relationship): PathPieces =
    PathPieces(this, Seq(), Some(relationship))

  def -->(node: Node): PathPieces =
    PathPieces(this, Seq(PathPiece(RightArrow, node)), None)

  def toQuery(identifiableMap: IdentifiableMap): String =
    arrow.toQuery(identifiableMap, relationship) + node.toQuery(identifiableMap)

}

object PathPiece {
  def apply(
    arrow: Arrow,
    node: Node,
    relationship: Relationship
  ): PathPiece =
    PathPiece(arrow, node, Some(relationship))
}
