package com.originate.scalypher

import com.originate.scalypher.action.ActionNodeOrRelationship
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.NodeOrRelationshipReference

package object types {
  trait Identifiable

  trait NodeOrRelationship extends Identifiable {
    def as(name: String): ActionNodeOrRelationship =
      ActionNodeOrRelationship(ObjectReference(this), Some(name))
  }

  object NodeOrRelationship {
    implicit def toReference(identifiable: NodeOrRelationship): NodeOrRelationshipReference =
      ObjectReference(identifiable)

    implicit def toActionReference(identifiable: NodeOrRelationship): ActionNodeOrRelationship =
      ActionNodeOrRelationship(identifiable)
  }

  type IdentifiableMap = Map[Identifiable, String]
}
