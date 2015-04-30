package com.scalypher

package object types {
  trait Referenceable

  type ReferenceableMap = Map[Referenceable, String]
}
