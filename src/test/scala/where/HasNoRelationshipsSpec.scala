package com.originate.scalypher.test.where

import com.originate.scalypher.where.HasNoRelationships
import com.originate.scalypher.path.{AnyNode, CypherNode}
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class HasNoRelationshipsSpec extends WordSpec with Matchers {

  "return a query to find AnyNodes with no relationships" in {
    val startNode = AnyNode()
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = HasNoRelationships(startNode)
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

  "return a query to find CypherNodes with no relationships" in {
    val startNode = CypherNode("name")
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = HasNoRelationships(startNode)
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

}
