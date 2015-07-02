package com.originate.scalypher.path

case class DanglingRelationship(path: Path, relationship: Relationship) {
  def --(node: Node): Path =
    path --(relationship, node)

  def -->(node: Node): Path =
    path -->(relationship, node)
}
