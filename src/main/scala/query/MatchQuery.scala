package com.originate.scalypher

import action.Action
import path.Path
import types._
import where.Where

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
