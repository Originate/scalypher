package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery

sealed trait BooleanOperator extends ToQuery
case object And extends ConstantString("AND") with BooleanOperator
case object Or extends ConstantString("OR") with BooleanOperator
case object Xor extends ConstantString("XOR") with BooleanOperator
