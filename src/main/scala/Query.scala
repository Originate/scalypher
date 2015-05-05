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

  def getReturnColumn: String =
    action match {
      case ReturnAll => ???
      case Delete(_, _) => ""
      case _ =>
        if (action.referenceables.size == 1) {
          val identifier = for {
            referenceable <- action.referenceables.headOption
            identifier <- referenceableMap get referenceable
          } yield identifier

          identifier getOrElse (throw new IdentifierDoesntExistException())
        }
        else ???
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
    val whereReferenceables = where map (_.referenceables) getOrElse Set()
    val referenceIdentifiers = (whereReferenceables ++ action.referenceables) map { referenceable =>
      (referenceable, nextIdentifier)
    }

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
