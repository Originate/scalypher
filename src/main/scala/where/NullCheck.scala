package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery

sealed trait NullCheck extends ToQuery
case object IsNull extends ConstantString("IS NULL") with NullCheck
case object NotNull extends ConstantString("IS NOT NULL") with NullCheck
