package com.originate.scalypher.test.path

import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.Property
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class NodeSpec extends WordSpec with Matchers {

  val emptyMap: IdentifiableMap = Map.empty

  "creating CypherNodes" must {

    "allow passing a single property" in {
      CypherNode("a" -> "b").toQuery(emptyMap) shouldBe """({a:"b"})"""
    }

    "allow passing a single label" in {
      CypherNode("name").toQuery(emptyMap) shouldBe """(:name)"""
    }

    "allow passing a single label and properties" in {
      val node = CypherNode("name", "a" -> "b", "c" -> 2)
      node.toQuery(emptyMap) shouldBe """(:name{a:"b",c:2})"""
    }

    "allow passing multiple labels and properties" in {
      val properties: Seq[Property] =  Seq("a" -> "b", "c" -> 2)
      val node = CypherNode(Seq("name1", "name2"), properties)
      node.toQuery(emptyMap) shouldBe """(:name1:name2{a:"b",c:2})"""
    }

  }

}
