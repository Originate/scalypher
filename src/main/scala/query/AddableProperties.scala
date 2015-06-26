package com.originate.scalypher

import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.where.ObjectReference

trait AddableProperties { self: Referenceable =>
  def assign(properties: Property*): MergePropertiesAssignment =
    MergePropertiesAssignment(ObjectReference(this), properties)
}
