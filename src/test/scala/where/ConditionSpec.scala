package com.originate.scalypher.test.where

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class ConditionSpec extends WordSpec with Matchers {

  "given a SeqValueReference" must {

    "serialize with IN" in {
      val node = AnyNode()
      (node.property("test") in Seq("a", "b")).toQuery(Map(node -> "a")) shouldBe """a.test IN ["a", "b"]"""
    }

  }

  "given multiple AnyNodes" must {

    val node1 = AnyNode()
    val node2 = AnyNode()
    val node3 = AnyNode()

    "generate a identifiableMap with unique identifiers for each" in {
      val where = Comparison(node1.property("test"), Equal, node2.property("test")) and
        Comparison(node3.property("test"), NotEqual, node1.property("test"))

      where.identifiables.size shouldBe 3
    }

  }

  "given multiple Nodes that are equivalent but different objects" must {

    val node1 = CypherNode("a")
    val node2 = CypherNode("a")
    val node3 = CypherNode("a")

    "generate a identifiableMap with unique identifiers for each" in {
      val where = Comparison(node1.property("test"), Equal, node2.property("test")) and
        Comparison(node3.property("test"), NotEqual, node1.property("test"))

      where.identifiables.size shouldBe 3
    }

  }

  "given multiple Relationships that are equivalent but different objects" must {

    val relationship1 = KindRelationship("a")
    val relationship2 = KindRelationship("a")
    val relationship3 = KindRelationship("a")

    "generate a identifiableMap with unique identifiers for each" in {
      val where = Comparison(relationship1.property("label"), Equal, relationship2.property("label")) and
        Comparison(relationship2.property("label"), NotEqual, relationship3.property("label"))

      where.identifiables.size shouldBe 3
    }

  }

}
