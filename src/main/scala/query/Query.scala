package com.originate.scalypher

import com.originate.scalypher.action.Action
import com.originate.scalypher.action.Delete
import com.originate.scalypher.action.ReferenceListAction
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
import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.Where

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
      case referenceListAction: ReferenceListAction => referenceListAction.returnColumns(referenceableMap)
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
