package com.originate.scalypher

import com.originate.scalypher.action.ReturnAction
import com.originate.scalypher.path.Path
import com.originate.scalypher.types._

case class MergeQuery(
  mergePath: Path,
  matchPaths: Seq[Path] = Seq.empty,
  createProperties: Seq[SetProperty] = Seq.empty,
  mergeProperties: Seq[SetProperty] = Seq.empty,
  returnAction: Option[ReturnAction] = None
) extends MatchCreateQuery {

  def onCreate(properties: Seq[SetProperty]): MergeQuery =
    copy(createProperties = properties)

  def onMerge(properties: Seq[SetProperty]): MergeQuery =
    copy(mergeProperties = properties)

  def onCreateAndMerge(properties: Seq[SetProperty]): MergeQuery =
    copy(createProperties = properties, mergeProperties = properties)

  def toQuery: String = {
    val matchString = ifNonEmpty(matchPaths) { paths =>
      stringListWithPrefix("MATCH", matchPaths map (_.toQuery(identifiableMap)))
    }
    val mergeString = Some(s"MERGE " + cleanedCreatePath.toQuery(modifiedReferenceableMap))
    val onCreateString = ifNonEmpty(createProperties) { properties =>
      stringListWithPrefix("ON CREATE SET", properties map (_.toQuery(identifiableMap)))
    }
    val onMergeString = ifNonEmpty(mergeProperties) { properties =>
      stringListWithPrefix("ON MATCH SET", properties map (_.toQuery(identifiableMap)))
    }
    val returnString = returnAction map (_.toQuery(identifiableMap))

    buildQuery(
      matchString,
      mergeString,
      onCreateString,
      onMergeString,
      returnString
    )
  }

  protected def withReturnAction(action: ReturnAction): MergeQuery =
    copy(returnAction = Some(action))

  private val onCreateOrMergeReferenceables =
    createProperties.flatMap(_.getReferenceable).toSet ++
      mergeProperties.flatMap(_.getReferenceable).toSet

  protected override val forcedCreateReferenceables: Set[Identifiable] = {
    val returnReferenceables = returnAction map (_.identifiables) getOrElse Set.empty
    onCreateOrMergeReferenceables ++ returnReferenceables
  }

  protected val identifiableMap: ReferenceableMap =
    referenceableMapWithPathWhereAndAction(
      matchPaths,
      None,
      returnAction,
      mergePath.identifiables - mergePath ++ onCreateOrMergeReferenceables
    )

  protected val (cleanedCreatePath, createMap) =
    cleanPathAndExtractMap(mergePath, matchPaths)

}
