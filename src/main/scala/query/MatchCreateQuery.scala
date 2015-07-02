package com.originate.scalypher

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ReturnAction
import com.originate.scalypher.action.ReturnAll
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.path.Node
import com.originate.scalypher.path.Path
import com.originate.scalypher.path.Relationship
import com.originate.scalypher.types._
import com.originate.scalypher.where.Reference

trait MatchCreateQuery extends Query {

  def returnAction: Option[ReturnAction]

  def returns(reference: ActionItem, rest: ActionItem*): MatchCreateQuery =
    withReturnAction(ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ActionItem, rest: ActionItem*): MatchCreateQuery =
    withReturnAction(ReturnDistinct(reference, rest: _*))

  def returnAll: MatchCreateQuery =
    withReturnAction(ReturnAll)

  def getReturnColumns: Set[String] =
    returnAction map matchActionToReturnColumns getOrElse Set.empty

  protected def withReturnAction(action: ReturnAction): MatchCreateQuery

  protected def forcedCreateReferenceables: Set[Identifiable]

  protected def modifiedReferenceableMap: IdentifiableMap = {
    val forcedMap = identifiableMap filterKeys (forcedCreateReferenceables contains _)
    createMap ++ forcedMap
  }

  protected def cleanedCreatePath: Path

  protected def createMap: IdentifiableMap

  protected def cleanPathAndExtractMap(path: Path, matchPaths: Seq[Path]): (Path, IdentifiableMap) = {
    val overlapReferenceables = matchPaths flatMap (_.identifiables) intersect path.identifiables.toSeq
    val relevantMap = identifiableMap filterKeys { key =>
      overlapReferenceables contains key
    }

    val pathTransform = relevantMap.foldLeft(PathTranform(path)) { case (acc @ PathTranform(path, map), (identifiable, identifier)) =>
      identifiable match {
        case node: Node =>
          val newNode = AnyNode()
          PathTranform(path.replaceNode(node, newNode), map - identifiable + (newNode -> identifier))
        case relationship: Relationship =>
          val newRelationship = AnyRelationship()
          PathTranform(path.replaceRelationship(relationship, newRelationship), map - identifiable + (newRelationship -> identifier))
        case _ =>
          acc
      }
    }

    (pathTransform.path, pathTransform.map)
  }

  private case class PathTranform(path: Path, map: IdentifiableMap = Map[Identifiable, String]())

}
