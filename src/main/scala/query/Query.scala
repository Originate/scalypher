package com.originate.scalypher

import action.Action
import action.Delete
import action.ReturnAction
import action.ReturnAll
import action.ReturnDistinct
import action.ReturnReference
import path.AnyNode
import path.AnyRelationship
import path.NodeType
import path.Path
import path.RelationshipType
import types._
import util.Exceptions.IdentifierDoesntExistException
import where.ReferenceType
import where.Where

sealed trait Query extends ToQuery {

  def getReturnColumns: Set[String]

  protected def referenceableMap: ReferenceableMap

  def getIdentifier(referenceable: Referenceable): Option[String] =
    referenceableMap get referenceable

  protected def ifNonEmpty[T](seq: Seq[T])(f: Seq[T] => String): Option[String] =
    if (seq.isEmpty) None
    else Some(f(seq))

  protected def stringListWithPrefix(prefix: String, strings: Seq[String]): String =
    s"""$prefix ${strings mkString ", "}"""

  protected var identifierIndex = 0
  protected def nextIdentifier: String = {
    identifierIndex += 1
    s"a$identifierIndex"
  }

  protected def buildQuery(strings: Option[String]*): String =
    strings.flatten mkString " "

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

trait MatchCreateQuery extends Query {

  def returnAction: Option[ReturnAction]

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

case class MergeQuery(
  mergePath: Path,
  matchPaths: Seq[Path] = Seq.empty,
  createProperties: Seq[SetProperty] = Seq.empty,
  mergeProperties: Seq[SetProperty] = Seq.empty,
  returnAction: Option[ReturnAction] = None
) extends MatchCreateQuery {

  def toQuery: String = {
    val matchString = ifNonEmpty(matchPaths) { paths =>
      stringListWithPrefix("MATCH", matchPaths map (_.toQuery(referenceableMap)))
    }
    val mergeString = Some(s"MERGE " + cleanedCreatePath.toQuery(modifiedReferenceableMap))
    val onCreateString = ifNonEmpty(createProperties) { properties =>
      stringListWithPrefix("ON CREATE SET", properties map (_.toQuery(referenceableMap)))
    }
    val onMergeString = ifNonEmpty(mergeProperties) { properties =>
      stringListWithPrefix("ON MERGE SET", properties map (_.toQuery(referenceableMap)))
    }
    val returnString = returnAction map (_.toQuery(referenceableMap))

    buildQuery(
      matchString,
      mergeString,
      onCreateString,
      onMergeString,
      returnString
    )
  }

  private val onCreateOrMergeReferenceables =
    createProperties.flatMap(_.getReferenceable).toSet ++
      mergeProperties.flatMap(_.getReferenceable).toSet

  protected val referenceableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(
      matchPaths,
      None,
      returnAction,
      mergePath.referenceables - mergePath ++ onCreateOrMergeReferenceables
    )

  protected val (cleanedCreatePath, createMap) =
    cleanPathAndExtractMap(mergePath, matchPaths)

}


case class CreateQuery(
  createPath: Path,
  matchPaths: Seq[Path] = Seq.empty,
  where: Option[Where] = None,
  returnAction: Option[ReturnAction] = None
) extends MatchCreateQuery {

  def returns(reference: ReferenceType, rest: ReferenceType*): CreateQuery =
    copy(returnAction = Some(ReturnReference(reference, rest: _*)))

  def returnDistinct(reference: ReferenceType, rest: ReferenceType*): CreateQuery =
    copy(returnAction = Some(ReturnDistinct(reference, rest: _*)))

  def returnAll: CreateQuery =
    copy(returnAction = Some(ReturnAll))

  def toQuery: String = {
    val matchString = ifNonEmpty(matchPaths) { paths =>
      stringListWithPrefix("MATCH", matchPaths map (_.toQuery(referenceableMap)))
    }
    val whereString = matchString flatMap { _ =>
      where map ("WHERE " + _.toQuery(referenceableMap))
    }
    val createString = Some("CREATE " + cleanedCreatePath.toQuery(modifiedReferenceableMap))
    val returnString = returnAction map (_.toQuery(referenceableMap))

    buildQuery(
      matchString,
      whereString,
      createString,
      returnString
    )
  }

  protected val referenceableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(
      matchPaths,
      where,
      returnAction,
      createPath.referenceables - createPath
    )

  protected val (cleanedCreatePath, createMap) =
    cleanPathAndExtractMap(createPath, matchPaths)

}

case class MatchQuery(pathMatch: Path, where: Option[Where], action: Action) extends Query {

  def getReturnColumns: Set[String] =
    matchActionToReturnColumns(action)

  def toQuery: String =
    buildQuery(
      Some("MATCH " + pathMatch.toQuery(referenceableMap)),
      where map ("WHERE " + _.toQuery(referenceableMap)),
      Some(action.toQuery(referenceableMap))
    )

  protected val referenceableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(Seq(pathMatch), where, Some(action))

}

object MatchQuery {

  def apply(pathMatch: Path, where: Where, action: Action): MatchQuery =
    MatchQuery(pathMatch, Some(where), action)

  def apply(pathMatch: Path, action: Action): MatchQuery =
    MatchQuery(pathMatch, None, action)

}
