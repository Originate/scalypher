package com.originate.scalypher

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ReturnAction
import com.originate.scalypher.action.ReturnAll
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.path.Path
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.Where

case class SetQuery(
  pathMatch: Path,
  where: Option[Where],
  assignments: Seq[Assignment],
  action: Option[ReturnAction]
) extends Query {

  def returns(reference: ActionItem, rest: ActionItem*): Query =
    copy(action = Some(ReturnReference(reference, rest: _*)))

  def returnDistinct(reference: ActionItem, rest: ActionItem*): Query =
    copy(action = Some(ReturnDistinct(reference, rest: _*)))

  def returnAll: Query =
    copy(action = Some(ReturnAll))

  def getReturnColumns: Set[String] =
    action map matchActionToReturnColumns getOrElse Set.empty

  def toQuery: String =
    buildQuery(
      Some("MATCH " + pathMatch.toQuery(identifiableMap)),
      where map ("WHERE " + _.toQuery(identifiableMap)),
      Some("SET " + (assignments map (_.toQuery(identifiableMap)) mkString ", ")),
      action map (_.toQuery(identifiableMap))
    )

  protected val identifiableMap: IdentifiableMap =
    identifiableMapWithPathWhereAndAction(
      Seq(pathMatch),
      where,
      action,
      assignments.flatMap(_.referenceables).toSet
    )

}

object SetQuery {

  def apply(pathMatch: Path, assignments: Assignment*): SetQuery =
    SetQuery(pathMatch, None, assignments, None)

  def apply(pathMatch: Path, where: Where, assignments: Assignment*): SetQuery =
    SetQuery(pathMatch, Some(where), assignments, None)

  def apply(pathMatch: Path, where: Option[Where], assignments: Assignment*): SetQuery =
    SetQuery(pathMatch, where, assignments, None)

  def apply(
    pathMatch: Path,
    where: Where,
    action: ReturnAction,
    assignments: Assignment*
  ): SetQuery =
    SetQuery(pathMatch, Some(where), assignments, Some(action))

}
