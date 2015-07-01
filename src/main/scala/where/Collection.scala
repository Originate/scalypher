package com.originate.scalypher.where

import com.originate.scalypher.ConstantString
import com.originate.scalypher.ToQuery
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.path.Path

sealed trait Projection extends ToQuery {
  def apply(path: Path): PathProjection =
    PathProjection(this, path)
}

case object Relationships extends ConstantString("RELATIONSHIPS") with Projection
case object Nodes extends ConstantString("NODES") with Projection

sealed trait Collection {
  def toQuery(identifiableMap: IdentifiableMap): String
  def identifiables: Set[Identifiable]
}

case class PathProjection(projection: Projection, path: Path) extends Collection {
  def toQuery(identifiableMap: IdentifiableMap): String =
    s"${projection.toQuery}(${identifiableMap(path)})"

  def identifiables: Set[Identifiable] =
    Set(path)
}
