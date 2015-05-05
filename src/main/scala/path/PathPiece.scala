package com.originate.scalypher.path

import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap

case class PathPieces(startPiece: PathPiece, tail: Seq[PathPiece], relationship: Option[RelationshipType]) {
  def --(node: NodeType): PathPieces =
    copy(tail = tail :+ PathPiece(DirectionlessArrow, node, relationship), relationship = None)

  def -->(node: NodeType): PathPieces =
    copy(tail = tail :+ PathPiece(RightArrow, node, relationship), relationship = None)
}

object PathPieces {
  def apply(pathPiece: PathPiece, relationship: RelationshipType): PathPieces =
    PathPieces(pathPiece, Seq(), Some(relationship))
}

case class PathPiece(
  arrow: ArrowType,
  node: NodeType,
  relationship: Option[RelationshipType] = None
) {
  def --(node: NodeType): PathPieces =
    PathPieces(this, Seq(PathPiece(DirectionlessArrow, node)), None)

  def --(relationship: RelationshipType): PathPieces =
    PathPieces(this, Seq(), Some(relationship))

  def -->(node: NodeType): PathPieces =
    PathPieces(this, Seq(PathPiece(RightArrow, node)), None)

  def toQuery(referenceableMap: ReferenceableMap): String =
    arrow.toQuery(referenceableMap, relationship) + node.toQuery(referenceableMap)
}

object PathPiece {
  def apply(
    arrow: ArrowType,
    node: NodeType,
    relationship: RelationshipType
  ): PathPiece =
    PathPiece(arrow, node, Some(relationship))
}