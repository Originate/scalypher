package com.originate.scalypher.test.query

import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.Property
import com.originate.scalypher.Label
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.RemovePropertyAssignment
import com.originate.scalypher.MergePropertiesAssignment
import com.originate.scalypher.OverwriteAssignment
import com.originate.scalypher.PropertyAssignment

import org.scalatest._

class AssignmentSpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val endNode = AnyNode()
  val relationship = AnyRelationship()

  val label1 = Label("label1")
  val label2 = Label("label2")
  val properties = Seq(
    Property("name", "matt"),
    Property("age", 12)
  )

  val map: ReferenceableMap = Map(
    startNode -> "a1",
    endNode -> "a2",
    relationship -> "a3"
  )

  "removing properties" must {

    "set elements to null" in {
      val assignment = startNode.property("name").assignNull
      assignment.toQuery(map) shouldBe "a1.name = NULL"
    }

  }

  "assigning properties" must {

    "set elements to strings" in {
      val assignment = startNode.property("name") assign "asdf"
      assignment.toQuery(map) shouldBe """a1.name = "asdf""""
    }

    "set elements to numbers" in {
      val assignment = startNode.property("name") assign 12
      assignment.toQuery(map) shouldBe "a1.name = 12"
    }

  }

  "merging properties" must {

    "set nodes to properties" in {
      val assignment = startNode assign (properties: _*)
      assignment.toQuery(map) shouldBe """a1 += {name:"matt",age:12}"""
    }

    "set relationships to properties" in {
      val assignment = relationship assign (properties: _*)
      assignment.toQuery(map) shouldBe """a3 += {name:"matt",age:12}"""
    }

    "retain ordering on properties" in {
      val properties = Seq(Property("name", "matt"), Property("name", "andy"))
      val assignment = relationship assign (properties: _*)
      assignment.toQuery(map) shouldBe """a3 += {name:"matt",name:"andy"}"""
    }

  }

  "overwriting all properties" must {

    "assign an object to another object" in {
      val assignment = startNode assign endNode
      assignment.toQuery(map) shouldBe "a1 = a2"
    }

  }

}
