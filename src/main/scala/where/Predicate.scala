package com.scalypher.where

import com.scalypher.ConstantString
import com.scalypher.ToQuery

sealed trait Predicate extends ToQuery
case object All extends ConstantString("ALL") with Predicate
case object Any extends ConstantString("ANY") with Predicate
case object NonePredicate extends ConstantString("NONE") with Predicate
case object Single extends ConstantString("SINGLE") with Predicate
