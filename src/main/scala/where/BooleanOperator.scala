package com.scalypher.where

import com.scalypher.ConstantString
import com.scalypher.ToQuery

sealed trait BooleanOperator extends ToQuery
case object And extends ConstantString("AND") with BooleanOperator
case object Or extends ConstantString("OR") with BooleanOperator
case object Xor extends ConstantString("XOR") with BooleanOperator
