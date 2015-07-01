package com.originate.scalypher

import com.originate.scalypher.action.ActionItem
import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.where.ObjectReference
import com.originate.scalypher.where.ReferenceableReference

import scala.language.implicitConversions

package object types {

  sealed trait Identifiable

  trait Referenceable extends Identifiable {
    def as(name: String): ActionReference =
      ActionReference(ObjectReference(this), Some(name))
  }

  trait NonReferenceable extends Identifiable

  object Referenceable {
    implicit def toReference(referenceable: Referenceable): ReferenceableReference =
      ObjectReference(referenceable)

    implicit def toActionReference(referenceable: Referenceable): ActionReference =
      ActionReference(referenceable)
  }

  type IdentifiableMap = Map[Identifiable, String]

}
