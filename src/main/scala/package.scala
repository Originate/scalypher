package com.originate.scalypher

import where.ObjectReference
import where.ReferenceType

package object types {
  trait Referenceable

  object Referenceable {
    implicit def toReference(referenceable: Referenceable): ReferenceType =
      ObjectReference(referenceable)
  }

  type ReferenceableMap = Map[Referenceable, String]
}
