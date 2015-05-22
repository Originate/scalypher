package com.originate.scalypher

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import path.Path
import path.NodeType
import path.AnyNode
import path.RelationshipType
import path.AnyRelationship
import where.Where
import action.Action
import action.ReturnAction
import action.ReturnAll
import action.Delete
import types._

sealed trait Query extends ToQuery {
  def getReturnColumns: Set[String]
  protected def referenceableMap: ReferenceableMap

  def getIdentifier(referenceable: Referenceable): Option[String] =
    referenceableMap get referenceable

  protected var identifierIndex = 0
  protected def nextIdentifier: String = {
    identifierIndex += 1
    s"a$identifierIndex"
  }

  protected def matchActionToReturnColumns(action: Action): Set[String] =
    action match {
      case ReturnAll => referenceableMap.values.toSet
      case _: Delete => Set.empty
      case _ =>
        action.referenceables map { referenceable =>
          referenceableMap get referenceable getOrElse (throw new IdentifierDoesntExistException())
        }
    }

  protected def referenceableMapWithPathWhereAndAction(
    paths: Seq[Path],
    where: Option[Where],
    action: Option[Action],
    forcedReferenceables: Set[Referenceable] = Set.empty
  ): ReferenceableMap = {
    val whereReferenceables = where map (_.referenceables) getOrElse Set()
    val referenceables = action match {
      case Some(ReturnAll) => paths flatMap (_.referenceables)
      case Some(action) => whereReferenceables ++ action.referenceables
      case _ => whereReferenceables
    }
    val referenceIdentifiers = (referenceables ++ forcedReferenceables) map ((_, nextIdentifier))

    referenceIdentifiers.toMap
  }

}

object Query {

  def toQueryWithProperty(
    referenceableMap: ReferenceableMap,
    referenceable: Referenceable,
    property: Option[PropertyReference] = None
  ): String = {
    val identifier = referenceableMap.get(referenceable) getOrElse (throw new IdentifierDoesntExistException())
    val propertyString = property map (p => s".${p.name}") getOrElse ""
    s"$identifier$propertyString"
  }

}

case class CreateQuery(
  createPath: Path,
  matchPaths: Seq[Path],
  where: Option[Where],
  returnAction: Option[ReturnAction]
) extends Query {

  def toQuery: String = {
    val matchString =
      if (matchPaths.isEmpty) ""
      else {
        val pathString = matchPaths map (_.toQuery(referenceableMap)) mkString ", "
        val whereString = where map (" WHERE " + _.toQuery(referenceableMap) + " ") getOrElse " "
        s"MATCH $pathString$whereString"
      }

    val createString = cleanedCreatePath.toQuery(createMap)
    s"${matchString}CREATE $createString"
  }

  def getReturnColumns: Set[String] =
    returnAction match {
      case Some(action) => matchActionToReturnColumns(action)
      case _ => Set.empty
    }

  protected val referenceableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(
      matchPaths,
      where,
      returnAction,
      createPath.referenceables - createPath
    )

  protected val PathTranform(cleanedCreatePath, createMap) = {
    val overlapReferenceables = matchPaths flatMap (_.referenceables) intersect createPath.referenceables.toSeq
    val relevantMap = referenceableMap filterKeys { key =>
      overlapReferenceables contains key
    }

    relevantMap.foldLeft(PathTranform(createPath)) { case (acc @ PathTranform(path, map), (referenceable, identifier)) =>
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
  }

  private case class PathTranform(path: Path, map: ReferenceableMap = Map[Referenceable, String]())

}

case class MatchQuery(pathMatch: Path, where: Option[Where], action: Action) extends Query {

  def getReturnColumns: Set[String] =
    matchActionToReturnColumns(action)

  def toQuery: String = {
    val pathString = pathMatch.toQuery(referenceableMap)
    val whereString = where map (" WHERE " + _.toQuery(referenceableMap) + " ") getOrElse " "
    val returnString = action.toQuery(referenceableMap)

    s"MATCH $pathString$whereString$returnString"
  }

  protected val referenceableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(Seq(pathMatch), where, Some(action))

}

object MatchQuery {

  def apply(pathMatch: Path, where: Where, action: Action): MatchQuery =
    MatchQuery(pathMatch, Some(where), action)

  def apply(pathMatch: Path, action: Action): MatchQuery =
    MatchQuery(pathMatch, None, action)

}
