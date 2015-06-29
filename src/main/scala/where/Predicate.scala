package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.path.Path
import com.originate.scalypher.ToQuery

case class PredicateWithProjection(
  predicate: Predicate,
  projection: PathProjection
) {
  def where(f: ObjectReference => Where): PredicateCondition =
    PredicateCondition(predicate, projection, f)
}

sealed trait Predicate extends ToQuery

trait PluralProjectionPredicate extends Predicate {
  def nodesIn(path: Path): PredicateWithProjection =
    PredicateWithProjection(this, Nodes(path))

  def relationshipsIn(path: Path): PredicateWithProjection =
    PredicateWithProjection(this, Relationships(path))
}

trait SingularProjectionPredicate extends Predicate {
  def nodeIn(path: Path): PredicateWithProjection =
    PredicateWithProjection(this, Nodes(path))

  def relationshipIn(path: Path): PredicateWithProjection =
    PredicateWithProjection(this, Relationships(path))
}

case object All extends ConstantString("ALL") with PluralProjectionPredicate
case object Any extends ConstantString("ANY") with PluralProjectionPredicate
case object No extends ConstantString("NONE") with PluralProjectionPredicate
case object Single extends ConstantString("SINGLE") with SingularProjectionPredicate
