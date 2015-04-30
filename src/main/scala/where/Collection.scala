package com.scalypher.where

import com.scalypher.ConstantString
import com.scalypher.ToQuery
import com.scalypher.types.ReferenceableMap
import com.scalypher.types.Referenceable
import com.scalypher.path.Path

sealed trait Projection extends ToQuery
case object Relationships extends ConstantString("RELATIONSHIPS") with Projection
case object Nodes extends ConstantString("NODES") with Projection

sealed trait Collection {
  def toQuery(referenceableMap: ReferenceableMap): String
  def referenceables: Set[Referenceable]
}

case class PathProjection(projection: Projection, path: Path) extends Collection {
  def toQuery(referenceableMap: ReferenceableMap): String =
    s"${projection.toQuery}(${referenceableMap(path)})"

  def referenceables: Set[Referenceable] =
    Set(path)
}
