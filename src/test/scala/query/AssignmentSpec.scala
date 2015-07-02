package com.originate.scalypher.test.query

import com.originate.scalypher.types.IdentifiableMap
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

  val map: IdentifiableMap = Map(
    startNode -> "a1",
    endNode -> "a2",
    relationship -> "a3"
  )

  "removing properties" must {

    val assignment = startNode.property("name").assignNull

    "set elements to null" in {
      assignment.toQuery(map) shouldBe "a1.name = NULL"
    }

    "return identifiables" in {
      assignment.referenceables shouldBe Set(startNode)
    }

  }

  "assigning properties" must {

    "set elements to strings" in {
      val assignment = startNode.property("name") := "asdf"
      assignment.toQuery(map) shouldBe """a1.name = "asdf""""
    }

    "set elements to numbers" in {
      val assignment = startNode.property("name") := 12
      assignment.toQuery(map) shouldBe "a1.name = 12"
    }

    "return identifiables" in {
      val assignment = startNode.property("name") := 12
      assignment.referenceables shouldBe Set(startNode)
    }

  }

  "merging properties" must {

    "set nodes to properties" in {
      val assignment = startNode := (properties: _*)
      assignment.toQuery(map) shouldBe """a1 += {name:"matt",age:12}"""
    }

    "set relationships to properties" in {
      val assignment = relationship := (properties: _*)
      assignment.toQuery(map) shouldBe """a3 += {name:"matt",age:12}"""
    }

    "retain ordering on properties" in {
      val properties = Seq(Property("name", "matt"), Property("name", "andy"))
      val assignment = relationship := (properties: _*)
      assignment.toQuery(map) shouldBe """a3 += {name:"matt",name:"andy"}"""
    }

    "return identifiables" in {
      val assignment = startNode := (properties: _*)
      assignment.referenceables shouldBe Set(startNode)
    }

  }

  "overwriting all properties" must {

    val assignment = startNode := endNode

    "assign an object to another object" in {
      assignment.toQuery(map) shouldBe "a1 = a2"
    }

    "return identifiables" in {
      assignment.referenceables shouldBe Set(startNode, endNode)
    }

  }

}
