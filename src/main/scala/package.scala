package com.originate.scalypher

import where.ObjectReference
import where.Reference

package object types {
  trait Referenceable

  object Referenceable {
    implicit def toReference(referenceable: Referenceable): Reference =
      ObjectReference(referenceable)
  }

  type ReferenceableMap = Map[Referenceable, String]
}
