package com.originate.scalypher.test.where

import com.originate.scalypher.where.NoRelationshipCondition
import com.originate.scalypher.path.{AnyNode, CypherNode}
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class NoRelationshipConditionSpec extends WordSpec with Matchers {

  "return a query to find AnyNodes with no relationships" in {
    val startNode = AnyNode()
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = NoRelationshipCondition(startNode)
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

  "return a query to find CypherNodes with no relationships" in {
    val startNode = CypherNode("name")
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = NoRelationshipCondition(startNode)
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

}
