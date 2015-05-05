package com.originate.scalypher

package object types {
  trait Referenceable

  type ReferenceableMap = Map[Referenceable, String]
}
