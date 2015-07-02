package com.originate.scalypher.test.path

import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.MatchQuery
import com.originate.scalypher.Query
import com.originate.scalypher.where.All

import org.scalatest._

class PathSpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val returns = ReturnDistinct(startNode)

  "defining simple paths" must {

    "respect arrows" in {
      val path = startNode --> AnyNode() <-- AnyNode()
      MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)-->()<--() RETURN DISTINCT a1"
    }

    "respect directionless arrows" in {
      val path = startNode -- AnyNode() -- AnyNode()
      MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)--()--() RETURN DISTINCT a1"
    }

  }

  "defining paths with conditions" must {

    val path = startNode -- AnyNode() -- AnyNode()

    "allow some where conditions" in {
      (path where Some(startNode <> "a") returns startNode).toQuery shouldBe """MATCH (a1)--()--() WHERE a1 <> "a" RETURN a1"""
    }

    "allow none where conditions" in {
      (path where None returns startNode).toQuery shouldBe "MATCH (a1)--()--() RETURN a1"
    }

    "allow where functions to apply functions to the path" in {
      val path = startNode --> AnyNode() where { path =>
        All relationshipsIn path where (_.property("name") <> "matt")
      } returns startNode

      path.toQuery shouldBe """MATCH a1 = (a2)-->() WHERE ALL (x IN RELATIONSHIPS(a1) WHERE x.name <> "matt") RETURN a2"""
    }

  }

  "definining paths with relationships" must {

    val relationship1 = KindRelationship("a")

    "respect right arrows" in {
      val path = startNode -- relationship1 --> AnyNode()
      MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)-[:a]->() RETURN DISTINCT a1"
    }

    "respect left arrows" in {
      val path = startNode <-- relationship1 -- AnyNode()
      MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)<-[:a]-() RETURN DISTINCT a1"
    }

    "handle deep nesting regardless of '-' operator precedence" when {

      "the path extends to another node" in {
        val path = startNode <-- relationship1 -- AnyNode() -- AnyNode()
        MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)<-[:a]-()--() RETURN DISTINCT a1"
      }

      "the path extends to a relationship and then a node" in {
        val endNode = AnyNode()
        val path = startNode <-- relationship1 -- AnyNode() -- relationship1 -- endNode
        MatchQuery(path, ReturnDistinct(endNode)).toQuery shouldBe "MATCH ()<-[:a]-()-[:a]-(a1) RETURN DISTINCT a1"
      }

      "the path extends to another node and then a right arrow" in {
        val path = startNode <-- relationship1 -- AnyNode() --> AnyNode()
        MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)<-[:a]-()-->() RETURN DISTINCT a1"
      }

      "the path extends to another node and then a left arrow" in {
        val path = startNode <-- relationship1 -- AnyNode() <-- AnyNode()
        MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)<-[:a]-()<--() RETURN DISTINCT a1"
      }

      "the path is long and convoluted" in {
        val path = startNode <-- relationship1 -- AnyNode() <-- AnyNode() -- relationship1 --> AnyNode() -- relationship1 -- AnyNode() -- relationship1 -- AnyNode() --> startNode
        MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)<-[:a]-()<--()-[:a]->()-[:a]-()-[:a]-()-->(a1) RETURN DISTINCT a1"
      }

    }

  }

}
