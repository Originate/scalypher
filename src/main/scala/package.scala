package com.originate.scalypher

import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.Reference

package object types {
  trait Referenceable

  object Referenceable {
    implicit def toReference(referenceable: Referenceable): Reference =
      ObjectReference(referenceable)

    implicit def toActionReference(referenceable: Referenceable): ActionReference =
      ActionReference(referenceable)
  }

  type ReferenceableMap = Map[Referenceable, String]
}
