package com.originate.scalypher

import com.originate.scalypher.action.Action
import com.originate.scalypher.path.Path
import com.originate.scalypher.types._
import com.originate.scalypher.where.Where

case class MatchQuery(pathMatch: Path, where: Option[Where], action: Action) extends Query {

  def getReturnColumns: Set[String] =
    matchActionToReturnColumns(action)

  def toQuery: String =
    buildQuery(
      Some("MATCH " + pathMatch.toQuery(identifiableMap)),
      where map ("WHERE " + _.toQuery(identifiableMap)),
      Some(action.toQuery(identifiableMap))
    )

  protected val identifiableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(Seq(pathMatch), where, Some(action))

}

object MatchQuery {

  def apply(pathMatch: Path, where: Where, action: Action): MatchQuery =
    MatchQuery(pathMatch, Some(where), action)

  def apply(pathMatch: Path, action: Action): MatchQuery =
    MatchQuery(pathMatch, None, action)

}
