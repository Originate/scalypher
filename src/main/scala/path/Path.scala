package com.originate.scalypher.path

import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.Where
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.Delete
import com.originate.scalypher.Query

case class PathWithWhere(path: Path, where: Where) {
  def returns(referenceable: Referenceable): Query =
    Query(path, where, ReturnReference(referenceable))

  def returnDistinct(referenceable: Referenceable): Query =
    Query(path, where, ReturnDistinct(referenceable))

  def delete(referenceable: Referenceable): Query =
    Query(path, where, Delete(referenceable))
}

case class Path(start: NodeType, pieces: Seq[PathPiece] = Seq.empty) extends Referenceable {
  def where(whereClause: Where): PathWithWhere =
    PathWithWhere(this, whereClause)

  def where(whereFunction: Path => Where): PathWithWhere =
    PathWithWhere(this, whereFunction(this))

  def returns(referenceable: Referenceable): Query =
    Query(this, ReturnReference(referenceable))

  def returnDistinct(referenceable: Referenceable): Query =
    Query(this, ReturnDistinct(referenceable))

  def delete(referenceable: Referenceable): Query =
    Query(this, Delete(referenceable))

  def -->(node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(RightArrow, node))

  def -->(relationship: RelationshipType, node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(RightArrow, node, relationship))

  def <--(path: Path): Path =
    copy(pieces = (pieces :+ PathPiece(LeftArrow, path.start)) ++ path.pieces)

  def <--(node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(LeftArrow, node))

  def <--(relationship: RelationshipType, node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(LeftArrow, node, relationship))

  def <--(pathPiece: PathPiece): Path = {
    val fixedArrow = pathPiece.copy(arrow = LeftArrow)
    copy(pieces = pieces :+ fixedArrow)
  }

  def <--(pathPieces: PathPieces): Path = {
    val fixedArrow = pathPieces.startPiece.copy(arrow = LeftArrow)
    copy(pieces = (pieces :+ fixedArrow) ++ pathPieces.tail)
  }

  def --(node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(DirectionlessArrow, node))

  def --(relationship: RelationshipType, node: NodeType): Path =
    copy(pieces = pieces :+ PathPiece(DirectionlessArrow, node, relationship))

  def --(relationship: RelationshipType): DanglingRelationship =
    DanglingRelationship(this, relationship)

  def toQuery(referenceableMap: ReferenceableMap): String = {
    val pathIdentifier = referenceableMap get this map (i => s"$i = ") getOrElse ""

    pathIdentifier + start.toQuery(referenceableMap) + (pieces map (_.toQuery(referenceableMap)) mkString "")
  }
}

object Path {
  private[path] def getIdentifierOrEmptyString(referenceableMap: ReferenceableMap, referenceable: Referenceable): String =
    referenceableMap get referenceable getOrElse ""
}
