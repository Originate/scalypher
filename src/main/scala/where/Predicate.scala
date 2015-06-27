package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.path.Path
import com.originate.scalypher.ToQuery

sealed trait Predicate extends ToQuery {
  def nodes(path: Path)(f: ObjectReference => Where): PredicateCondition =
    PredicateCondition(this, Nodes(path), f)

  def relationships(path: Path)(f: ObjectReference => Where): PredicateCondition =
    PredicateCondition(this, Relationships(path), f)
}

case object All extends ConstantString("ALL") with Predicate
case object Any extends ConstantString("ANY") with Predicate
case object NonePredicate extends ConstantString("NONE") with Predicate
case object Single extends ConstantString("SINGLE") with Predicate
