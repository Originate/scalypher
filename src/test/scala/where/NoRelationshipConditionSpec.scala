package com.originate.scalypher.test.where

import com.originate.scalypher.where.NoRelationshipCondition
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.types.IdentifiableMap


import org.scalatest._

class NoRelationshipConditionSpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val identifiableMap: IdentifiableMap = Map(startNode -> "a1")

  "return a query to find nodes with no relationships" in {
    val condition = NoRelationshipCondition(startNode)
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

}
