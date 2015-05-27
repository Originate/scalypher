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
import where.Reference
import where.Where

trait Query extends ToQuery {

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
    property: Option[PropertyName] = None
  ): String = {
    val identifier = referenceableMap.get(referenceable) getOrElse (throw new IdentifierDoesntExistException())
    val propertyString = property map (p => s".${p.name}") getOrElse ""
    s"$identifier$propertyString"
  }

}
