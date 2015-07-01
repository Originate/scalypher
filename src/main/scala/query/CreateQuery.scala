package com.originate.scalypher

import com.originate.scalypher.action.ReturnAction
import com.originate.scalypher.path.Path
import com.originate.scalypher.types._
import com.originate.scalypher.where.Where


case class CreateQuery(
  createPath: Path,
  matchPaths: Seq[Path] = Seq.empty,
  where: Option[Where] = None,
  returnAction: Option[ReturnAction] = None
) extends MatchCreateQuery {

  def toQuery: String = {
    val matchString = ifNonEmpty(matchPaths) { paths =>
      stringListWithPrefix("MATCH", matchPaths map (_.toQuery(identifiableMap)))
    }
    val whereString =
      if (matchString.isEmpty) None
      else where map ("WHERE " + _.toQuery(identifiableMap))
    val createString = Some("CREATE " + cleanedCreatePath.toQuery(modifiedReferenceableMap))
    val returnString = returnAction map (_.toQuery(identifiableMap))

    buildQuery(
      matchString,
      whereString,
      createString,
      returnString
    )
  }

  protected val forcedCreateReferenceables: Set[Identifiable] =
    returnAction map (_.identifiables) getOrElse Set.empty

  protected def withReturnAction(action: ReturnAction): CreateQuery =
    copy(returnAction = Some(action))

  protected val identifiableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(
      matchPaths,
      where,
      returnAction,
      createPath.identifiables - createPath
    )

  protected val (cleanedCreatePath, createMap) =
    cleanPathAndExtractMap(createPath, matchPaths)

}
