package com.originate.scalypher

import action.ReturnAction
import path.Path
import types._
import where.Where


case class CreateQuery(
  createPath: Path,
  matchPaths: Seq[Path] = Seq.empty,
  where: Option[Where] = None,
  returnAction: Option[ReturnAction] = None
) extends MatchCreateQuery {

  def withReturnAction(action: ReturnAction): CreateQuery =
    copy(returnAction = Some(action))

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
