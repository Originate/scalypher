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
  val setQuery = path set (startNode.property("name") := "dan")

  "building a set query" must {

    "allow setting values" in {
      val query = path set (startNode.property("name") := "x")
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x""""
    }

    "allow setting multiple values" in {
      val query = path set (
        startNode.property("name") := "x",
        startNode.property("age") := 2
      )
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x", a1.age = 2"""
    }

    "retain ordering when setting multiple values on the same property" in {
      val query = path set (
        startNode.property("name") := "x",
        startNode.property("name") := "y"
      )
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x", a1.name = "y""""
    }

    "allow a where clause" in {
      val query = path where (startNode.property("name") <> 1) set (startNode.property("name") := "x")
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.name <> 1 SET a1.name = "x""""
    }

    "allow returns statements" in {
      val query = path set (startNode.property("name") := "x") returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() SET a1.name = "x" RETURN a1"""
    }

    "allow a where with a returns statement" in {
      val query = path where (
        startNode.property("name") <> 1
      ) set (
        startNode.property("name") := "x"
      ) returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.name <> 1 SET a1.name = "x" RETURN a1"""
    }

    "allow simple syntax for returnAll statements" in {
      setQuery.returnAll.toQuery shouldBe """MATCH a1 = (a4)-->(a3) SET a4.name = "dan" RETURN *"""
    }

    "allow simple syntax for returnDistinct statements" in {
      setQuery.returnDistinct(startNode).toQuery shouldBe """MATCH (a1)-->() SET a1.name = "dan" RETURN DISTINCT a1"""
    }

  }

  "getting return columns" when {

    "action is empty" must {

      "return an empty set" in {
        setQuery.getReturnColumns shouldBe 'empty
      }

    }

    "action is not empty" must {

      "return the return columns" in {
        setQuery.returnAll.getReturnColumns shouldNot be ('empty)
      }

    }

  }

}
