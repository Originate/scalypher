package com.originate.scalypher.path

import com.originate.scalypher.action.Delete
import com.originate.scalypher.action.ReturnAll
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.Query
import com.originate.scalypher.MatchQuery
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.where.ReferenceType
import com.originate.scalypher.where.Where

case class PathWithWhere(path: Path, where: Where) {
  def returns(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(path, where, ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(path, where, ReturnDistinct(reference, rest: _*))

  def returnAll: Query =
    MatchQuery(path, where, ReturnAll)

  def delete(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(path, where, Delete(reference, rest: _*))
}

case class Path(start: NodeType, pieces: Seq[PathPiece] = Seq.empty) extends Referenceable {
  private[scalypher] def referenceables: Set[Referenceable] = {
    val extraReferenceables = pieces flatMap { piece =>
      Seq(Some(piece.node), piece.relationship).flatten
    }

    Set(this, start) ++ extraReferenceables.toSet
  }

  def where(whereClause: Where): PathWithWhere =
    PathWithWhere(this, whereClause)

  def where(whereFunction: Path => Where): PathWithWhere =
    PathWithWhere(this, whereFunction(this))

  def returns(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(this, ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(this, ReturnDistinct(reference, rest: _*))

  def returnAll: Query =
    MatchQuery(this, ReturnAll)

  def delete(reference: ReferenceType, rest: ReferenceType*): Query =
    MatchQuery(this, Delete(reference, rest: _*))

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
