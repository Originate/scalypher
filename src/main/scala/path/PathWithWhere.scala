package com.originate.scalypher.path

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.Delete
import com.originate.scalypher.action.ReturnAll
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.Query
import com.originate.scalypher.MatchQuery
import com.originate.scalypher.MergeQuery
import com.originate.scalypher.SetQuery
import com.originate.scalypher.Assignment
import com.originate.scalypher.CreateQuery
import com.originate.scalypher.where.Reference
import com.originate.scalypher.where.Where

case class PathWithWhere(path: Path, where: Option[Where]) {
  def set(assignments: Assignment*): SetQuery =
    SetQuery(path, where, assignments: _*)

  def returns(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(path, where, ReturnReference(reference, rest: _*))

  def returnDistinct(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(path, where, ReturnDistinct(reference, rest: _*))

  def returnAll: Query =
    MatchQuery(path, where, ReturnAll)

  def delete(reference: ActionItem, rest: ActionItem*): Query =
    MatchQuery(path, where, Delete(reference, rest: _*))

  def create(createPath: Path): CreateQuery =
    CreateQuery(createPath, Seq(path), where)

  def merge(mergePath: Path): MergeQuery =
    MergeQuery(mergePath, Seq(path))
}

object PathWithWhere {
  def apply(path: Path, where: Where): PathWithWhere =
    PathWithWhere(path, Some(where))
}
