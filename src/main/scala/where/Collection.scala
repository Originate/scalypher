package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.path.Path

sealed trait Projection extends ToQuery {
  def apply(path: Path): PathProjection =
    PathProjection(this, path)
}

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
