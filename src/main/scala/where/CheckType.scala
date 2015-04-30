package com.scalypher.where

import com.scalypher.ConstantString
import com.scalypher.ToQuery

sealed trait CheckType extends ToQuery
case object IsNull extends ConstantString("IS NULL") with CheckType
case object NotNull extends ConstantString("IS NOT NULL") with CheckType
