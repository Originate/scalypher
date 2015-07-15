package com.originate.scalypher.test.where

import com.originate.scalypher.where.HasNoRelationships
import com.originate.scalypher.path.{AnyNode, CypherNode}
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class HasNoRelationshipsSpec extends WordSpec with Matchers {

  "return a query to find AnyNodes with no relationships" in {
    val startNode = AnyNode()
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = startNode.hasNoRelationships()
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

  "return a query to find CypherNodes with no relationships" in {
    val startNode = CypherNode("name")
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = startNode.hasNoRelationships()
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[]-()"
  }

  "return a query to find a node with no relationships that have a certain label" in {
    val startNode = AnyNode()
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = startNode.hasNoRelationships() withLabel "label"
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[:label]-()"
  }

  "return a query to find a node with no relationships that have multiple specified labels" in {
    val startNode = AnyNode()
    val identifiableMap: IdentifiableMap = Map(startNode -> "a1")
    val condition = startNode.hasNoRelationships(Seq("label1", "label2", "label3"))
    condition.toQuery(identifiableMap) shouldBe "NOT (a1)-[:label1|label2|label3]-()"
  }

}
