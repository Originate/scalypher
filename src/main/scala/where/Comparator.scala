package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery

sealed trait Comparator extends ToQuery
case object Equal extends ConstantString("=") with Comparator
case object NotEqual extends ConstantString("<>") with Comparator
case object GT extends ConstantString(">") with Comparator
case object GTE extends ConstantString(">=") with Comparator
case object LT extends ConstantString("<") with Comparator
case object LTE extends ConstantString("<=") with Comparator
case object IN extends ConstantString("IN") with Comparator
