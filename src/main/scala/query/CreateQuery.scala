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

  def toQuery: String = {
    val matchString = ifNonEmpty(matchPaths) { paths =>
      stringListWithPrefix("MATCH", matchPaths map (_.toQuery(referenceableMap)))
    }
    val whereString =
      if (matchString.isEmpty) None
      else where map ("WHERE " + _.toQuery(referenceableMap))
    val createString = Some("CREATE " + cleanedCreatePath.toQuery(modifiedReferenceableMap))
    val returnString = returnAction map (_.toQuery(referenceableMap))

    buildQuery(
      matchString,
      whereString,
      createString,
      returnString
    )
  }

  protected val forcedCreateReferenceables: Set[Referenceable] =
    returnAction map (_.referenceables) getOrElse Set.empty

  protected def withReturnAction(action: ReturnAction): CreateQuery =
    copy(returnAction = Some(action))

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
