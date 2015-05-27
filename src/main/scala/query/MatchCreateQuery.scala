package com.originate.scalypher

import action.ReturnAction
import action.ReturnAll
import action.ReturnDistinct
import action.ReturnReference
import path.AnyNode
import path.AnyRelationship
import path.NodeType
import path.Path
import path.RelationshipType
import where.ReferenceType
import types._

trait MatchCreateQuery extends Query {

  def withReturnAction(action: ReturnAction): MatchCreateQuery

  def returnAction: Option[ReturnAction]

  def returns(reference: ReferenceType, rest: ReferenceType*): MatchCreateQuery =
    withReturnAction(ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ReferenceType, rest: ReferenceType*): MatchCreateQuery =
    withReturnAction(ReturnDistinct(reference, rest: _*))

  def returnAll: MatchCreateQuery =
    withReturnAction(ReturnAll)

  def getReturnColumns: Set[String] =
    returnAction match {
      case Some(action) => matchActionToReturnColumns(action)
      case _ => Set.empty
    }

  protected def modifiedReferenceableMap: ReferenceableMap = {
    val returnReferenceables = returnAction map (_.referenceables) getOrElse Set.empty
    val returnMap = referenceableMap filterKeys (returnReferenceables contains _)
    createMap ++ returnMap
  }

  protected case class PathTranform(path: Path, map: ReferenceableMap = Map[Referenceable, String]())

  protected def cleanedCreatePath: Path

  protected def createMap: ReferenceableMap

  protected def cleanPathAndExtractMap(path: Path, matchPaths: Seq[Path]): (Path, ReferenceableMap) = {
    val overlapReferenceables = matchPaths flatMap (_.referenceables) intersect path.referenceables.toSeq
    val relevantMap = referenceableMap filterKeys { key =>
      overlapReferenceables contains key
    }

    val pathTransform = relevantMap.foldLeft(PathTranform(path)) { case (acc @ PathTranform(path, map), (referenceable, identifier)) =>
      referenceable match {
        case node: NodeType =>
          val newNode = AnyNode()
          PathTranform(path.replaceNode(node, newNode), map - referenceable + (newNode -> identifier))
        case relationship: RelationshipType =>
          val newRelationship = AnyRelationship()
          PathTranform(path.replaceRelationship(relationship, newRelationship), map - referenceable + (newRelationship -> identifier))
        case _ =>
          acc
      }
    }

    (pathTransform.path, pathTransform.map)
  }

}
