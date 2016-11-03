package com.originate.scalypher.path

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ActionPath
import com.originate.scalypher.action.Delete
import com.originate.scalypher.action.ReturnAll
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.Assignment
import com.originate.scalypher.CreateQuery
import com.originate.scalypher.MatchQuery
import com.originate.scalypher.MergeQuery
import com.originate.scalypher.Query
import com.originate.scalypher.SetQuery
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.NonReferenceable
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.Where

import scala.language.implicitConversions

case class Path(start: Node, pieces: Seq[PathPiece] = Seq.empty) extends NonReferenceable {

  def as(name: String): ActionItem =
    ActionPath(this, Some(name))

  def set(assignments: Assignment*): SetQuery =
    SetQuery(this, assignments: _*)

  def create(createPath: Path): CreateQuery =
    CreateQuery(createPath, Seq(this))

  def merge(mergePath: Path): MergeQuery =
    MergeQuery(mergePath, Seq(this))

  def where(whereClause: Where): PathWithWhere =
    PathWithWhere(this, whereClause)

  def where(whereClause: Option[Where]): PathWithWhere =
    PathWithWhere(this, whereClause)

  def where(whereFunction: Path => Where): PathWithWhere =
    PathWithWhere(this, whereFunction(this))

  def returns(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(this, ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(this, ReturnDistinct(reference, rest: _*))

  def returnAll: Query =
    MatchQuery(this, ReturnAll)

  def delete(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(this, Delete(reference, rest: _*))

  def -->(node: Node): Path =
    copy(pieces = pieces :+ PathPiece(RightArrow, node))

  def -->(relationship: Relationship, node: Node): Path =
    copy(pieces = pieces :+ PathPiece(RightArrow, node, relationship))

  def <--(path: Path): Path =
    copy(pieces = (pieces :+ PathPiece(LeftArrow, path.start)) ++ path.pieces)

  def <--(node: Node): Path =
    copy(pieces = pieces :+ PathPiece(LeftArrow, node))

  def <--(relationship: Relationship, node: Node): Path =
    copy(pieces = pieces :+ PathPiece(LeftArrow, node, relationship))

  def <--(pathPiece: PathPiece): Path = {
    val fixedArrow = pathPiece.copy(arrow = LeftArrow)
    copy(pieces = pieces :+ fixedArrow)
  }

  def <--(pathPieces: PathPieces): Path = {
    val fixedArrow = pathPieces.startPiece.copy(arrow = LeftArrow)
    copy(pieces = (pieces :+ fixedArrow) ++ pathPieces.tail)
  }

  def --(node: Node): Path =
    copy(pieces = pieces :+ PathPiece(DirectionlessArrow, node))

  def --(relationship: Relationship, node: Node): Path =
    copy(pieces = pieces :+ PathPiece(DirectionlessArrow, node, relationship))

  def --(relationship: Relationship): DanglingRelationship =
    DanglingRelationship(this, relationship)

  def toQuery(identifiableMap: IdentifiableMap): String = {
    val pathIdentifier = identifiableMap get this map (i => s"$i = ") getOrElse ""

    pathIdentifier + start.toQuery(identifiableMap) + (pieces map (_.toQuery(identifiableMap)) mkString "")
  }

  private[scalypher] def replaceNode(oldNode: Node, newNode: Node): Path = {
    val newPieces = pieces map (_.replaceNode(oldNode, newNode))

    if (start == oldNode) Path(newNode, newPieces)
    else copy(pieces = newPieces)
  }

  private[scalypher] def replaceRelationship(oldRelationship: Relationship, newRelationship: Relationship): Path =
    copy(pieces = pieces map (_.replaceRelationship(oldRelationship, newRelationship)))

  private[scalypher] def identifiables: Set[Identifiable] = {
    val extraReferenceables = pieces flatMap { piece =>
      Seq(Some(piece.node), piece.relationship).flatten
    }

    Set(this, start) ++ extraReferenceables.toSet
  }

}

object Path {
  private[path] def getIdentifierOrEmptyString(identifiableMap: IdentifiableMap, identifiable: Identifiable): String =
    identifiableMap get identifiable getOrElse ""

  implicit def toActionReference(path: Path): ActionItem =
    ActionPath(path)
}
