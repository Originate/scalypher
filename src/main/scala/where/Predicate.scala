package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery

sealed trait Predicate extends ToQuery
case object All extends ConstantString("ALL") with Predicate
case object Any extends ConstantString("ANY") with Predicate
case object NonePredicate extends ConstantString("NONE") with Predicate
case object Single extends ConstantString("SINGLE") with Predicate
