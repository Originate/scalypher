package com.originate.scalypher

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.ReferenceableReference

package object types {

  sealed trait Identifiable

  trait Referenceable extends Identifiable {
    def as(name: String): ActionReference =
      ActionReference(ObjectReference(this), Some(name))
  }

  trait NonReferenceable extends Identifiable

  object Referenceable {
    implicit def toReference(identifiable: Referenceable): ReferenceableReference =
      ObjectReference(identifiable)

    implicit def toActionReference(identifiable: Referenceable): ActionReference =
      ActionReference(identifiable)
  }

  type IdentifiableMap = Map[Identifiable, String]

}
