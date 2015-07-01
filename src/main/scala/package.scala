package com.originate.scalypher

import com.originate.scalypher.action.ActionNodeOrRelationship
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.NodeOrRelationshipReference

package object types {
  trait Referenceable

  trait NodeOrRelationship extends Referenceable {
    def as(name: String): ActionNodeOrRelationship =
      ActionNodeOrRelationship(ObjectReference(this), Some(name))
  }

  object NodeOrRelationship {
    implicit def toReference(referenceable: NodeOrRelationship): NodeOrRelationshipReference =
      ObjectReference(referenceable)

    implicit def toActionReference(referenceable: NodeOrRelationship): ActionNodeOrRelationship =
      ActionNodeOrRelationship(referenceable)
  }

  type ReferenceableMap = Map[Referenceable, String]
}
