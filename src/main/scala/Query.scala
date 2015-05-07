package com.originate.scalypher

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import path.Path
import where.Where
import action.Action
import action.ReturnAll
import action.Delete
import types._

abstract class ConstantString(stringified: String) {
  def toQuery: String = stringified
}

trait ToQuery {
  def toQuery: String
}

trait ToQueryWithIdentifiers {
  def toQuery(referenceableMap: ReferenceableMap): String
}

case class PropertyReference(name: String)

case class Query(pathMatch: Path, where: Option[Where], action: Action) {

  def getIdentifier(referenceable: Referenceable): Option[String] =
    referenceableMap get referenceable

  def getReturnColumns: Set[String] =
    action match {
      case ReturnAll => referenceableMap.values.toSet
      case _: Delete => Set.empty
      case _ =>
        action.referenceables map { referenceable =>
          referenceableMap get referenceable getOrElse (throw new IdentifierDoesntExistException())
        }
    }

  def toQuery: String = {
    val pathString = pathMatch.toQuery(referenceableMap)
    val whereString = where map (" WHERE " + _.toQuery(referenceableMap) + " ") getOrElse " "
    val returnString = action.toQuery(referenceableMap)

    s"MATCH $pathString$whereString$returnString"
  }

  private var identifierIndex = 0
  private def nextIdentifier: String = {
    identifierIndex += 1
    s"a$identifierIndex"
  }

  private val referenceableMap: ReferenceableMap = {
    val referenceables =
      if (action == ReturnAll) pathMatch.referenceables
      else {
        val whereReferenceables = where map (_.referenceables) getOrElse Set()
        (whereReferenceables ++ action.referenceables)
      }
    val referenceIdentifiers = referenceables map ((_, nextIdentifier))

    referenceIdentifiers.toMap
  }

}

object Query {

  def apply(pathMatch: Path, where: Where, action: Action): Query =
    Query(pathMatch, Some(where), action)

  def apply(pathMatch: Path, action: Action): Query =
    Query(pathMatch, None, action)

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
