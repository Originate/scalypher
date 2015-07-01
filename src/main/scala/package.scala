package com.originate.scalypher

import com.originate.scalypher.action.ActionNodeOrRelationship
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.NodeOrRelationshipReference

package object types {
  trait Identifiable

  trait Referenceable extends Identifiable {
    def as(name: String): ActionNodeOrRelationship =
      ActionNodeOrRelationship(ObjectReference(this), Some(name))
  }

  object Referenceable {
    implicit def toReference(identifiable: Referenceable): NodeOrRelationshipReference =
      ObjectReference(identifiable)

    implicit def toActionReference(identifiable: Referenceable): ActionNodeOrRelationship =
      ActionNodeOrRelationship(identifiable)
  }

  type IdentifiableMap = Map[Identifiable, String]
}
