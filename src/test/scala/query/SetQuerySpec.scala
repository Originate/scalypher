package com.originate.scalypher.test.query

import com.originate.scalypher.where.Comparison
import com.originate.scalypher.where.NotEqual
import com.originate.scalypher.where.Equal
import com.originate.scalypher.where.ValueReference
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.action.ReturnReference
import com.originate.scalypher.SetQuery

import org.scalatest._

class SetQuerySpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val path = startNode --> AnyNode()
  val returns = ReturnReference(startNode)

  "building a set query" must {

    "allow setting values" in {
      val query = path set (startNode.property("name") assign "x")
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x""""
    }

    "allow setting multiple values" in {
      val query = path set (
        startNode.property("name") assign "x",
        startNode.property("age") assign 2
      )
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x", a1.age = 2"""
    }

    "retain ordering when setting multiple values on the same property" in {
      val query = path set (
        startNode.property("name") assign "x",
        startNode.property("name") assign "y"
      )
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x", a1.name = "y""""
    }

    "allow a where clause" in {
      val query = path where (startNode.property("name") <> 1) set (startNode.property("name") assign "x")
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.name <> 1 SET a1.name = "x""""
    }

    "allow returns statements" in {
      val query = path set (startNode.property("name") assign "x") returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x" RETURN a1"""
    }

    "allow a where with a returns statement" in {
      val query = path where (
        startNode.property("name") <> 1
      ) set (
        startNode.property("name") assign "x"
      ) returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.name <> 1 SET a1.name = "x" RETURN a1"""
    }

  }

}
