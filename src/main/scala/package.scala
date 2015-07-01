package com.originate.scalypher

import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.NodeOrRelationshipReference

package object types {
  trait Identifiable

  trait Referenceable extends Identifiable {
    def as(name: String): ActionReference =
      ActionReference(ObjectReference(this), Some(name))
  }

  object Referenceable {
    implicit def toReference(identifiable: Referenceable): NodeOrRelationshipReference =
      ObjectReference(identifiable)

    implicit def toActionReference(identifiable: Referenceable): ActionReference =
      ActionReference(identifiable)
  }

  type IdentifiableMap = Map[Identifiable, String]
}
