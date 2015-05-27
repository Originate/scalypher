package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery

sealed trait CheckType extends ToQuery
case object IsNull extends ConstantString("IS NULL") with CheckType
case object NotNull extends ConstantString("IS NOT NULL") with CheckType
