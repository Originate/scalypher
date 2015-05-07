package com.originate.scalypher

import where.Reference
import where.ReferenceType

package object types {
  trait Referenceable

  object Referenceable {
    implicit def toReference(referenceable: Referenceable): ReferenceType =
      Reference(referenceable)
  }

  type ReferenceableMap = Map[Referenceable, String]
}
