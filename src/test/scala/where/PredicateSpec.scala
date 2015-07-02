package com.originate.scalypher.test.where

import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class PredicateConditionSpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val path = startNode --> AnyNode()
  val identifiableMap: IdentifiableMap = Map(path -> "path")

  "allow simple node conditions" in {
    val predicate = All nodesIn path where { node =>
      node.property("name") <> "matt"
    }

    predicate.toQuery(identifiableMap) shouldBe """ALL (x IN NODES(path) WHERE x.name <> "matt")"""
  }

  "allow singular node conditions" in {
    val predicate = Single nodeIn path where { node =>
      node.property("name") <> "matt"
    }

    predicate.toQuery(identifiableMap) shouldBe """SINGLE (x IN NODES(path) WHERE x.name <> "matt")"""
  }

  "allow simple relationship conditions" in {
    val predicate = All relationshipsIn path where { relationship =>
      relationship.property("name") <> "matt"
    }

    predicate.toQuery(identifiableMap) shouldBe """ALL (x IN RELATIONSHIPS(path) WHERE x.name <> "matt")"""
  }

  "allow referencing elements outside of the predicate condition" in {
    val map = identifiableMap + (startNode -> "node")
    val predicate = All nodesIn path where { node =>
      (node.property("name") <> startNode.property("name"))
    }

    predicate.toQuery(map) shouldBe """ALL (x IN NODES(path) WHERE x.name <> node.name)"""
  }

  "allow multiple conditions in the predicate" in {
    val predicate = All nodesIn path where { node =>
      (node.property("name") <> "matt") and
        (node.property("age") <> 12)
    }

    predicate.toQuery(identifiableMap) shouldBe """ALL (x IN NODES(path) WHERE x.name <> "matt" AND x.age <> 12)"""
  }

}
