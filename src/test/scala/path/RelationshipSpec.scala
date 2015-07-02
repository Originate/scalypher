package com.originate.scalypher.test.path

import com.originate.scalypher.path._
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.util.Exceptions.CharacterNotAllowedInLabel

import org.scalatest._

class RelationshipSpec extends WordSpec with Matchers {

  val Label = "label"
  val Label1 = "label1"
  val Label2 = "label2"

  def toQuery(relationship: Relationship): String =
    relationship.toQuery(Map(relationship -> "a1"))

  def toQueryEmpty(relationship: Relationship): String =
    relationship.toQuery(Map.empty)

  "CreateRelationship" must {

    "convert to query with a label" in {
      toQuery(CreateRelationship(Label)) shouldBe s"""[a1:$Label]"""
    }

    "convert to query without an identifier" in {
      val relationship = CreateRelationship(Label)
      relationship.toQuery(Map.empty) shouldBe s"""[:$Label]"""
    }

    "convert to query with a label and properties" in {
      toQuery(CreateRelationship(Label, Seq("a" -> 1))) shouldBe s"""[a1:$Label{a:1}]"""
    }

    "escape labels with spaces" in {
      toQuery(CreateRelationship("asdf asdf")) shouldBe s"""[a1:`asdf asdf`]"""
    }

    "throw an exception for invalid characters" in {
      a [CharacterNotAllowedInLabel] should be thrownBy toQuery(CreateRelationship("a`sdf"))
    }

  }

  "AnyRelationship" must {

    "convert to query" in {
      toQuery(AnyRelationship()) shouldBe """[a1]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(AnyRelationship()) shouldBe """[]"""
    }

  }

  "KindRelationship" must {

    "convert to query" in {
      toQuery(KindRelationship(Label)) shouldBe s"""[a1:$Label]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(KindRelationship(Label)) shouldBe s"""[:$Label]"""
    }

    "convert to query with multiple labels" in {
      toQuery(KindRelationship(Label1, Label2)) shouldBe s"""[a1:$Label1|$Label2]"""
    }

  }

  "DistanceRelationship" must {

    "convert to query" in {
      toQuery(DistanceRelationship(3)) shouldBe """[a1*3]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(DistanceRelationship(3)) shouldBe """[*3]"""
    }

    "convert to query with kinds" in {
      toQuery(DistanceRelationship(3, Label1, Label2)) shouldBe s"""[a1:$Label1|$Label2*3]"""
    }

  }

  "MaxDistanceRelationship" must {

    "convert to query" in {
      toQuery(MaxDistanceRelationship(3)) shouldBe """[a1*..3]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(MaxDistanceRelationship(3)) shouldBe """[*..3]"""
    }

    "convert to query with kinds" in {
      toQuery(MaxDistanceRelationship(3, Label1, Label2)) shouldBe s"""[a1:$Label1|$Label2*..3]"""
    }

  }

  "MinDistanceRelationship" must {

    "convert to query" in {
      toQuery(MinDistanceRelationship(3)) shouldBe """[a1*3..]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(MinDistanceRelationship(3)) shouldBe """[*3..]"""
    }

    "convert to query with kinds" in {
      toQuery(MinDistanceRelationship(3, Label1, Label2)) shouldBe s"""[a1:$Label1|$Label2*3..]"""
    }

  }

  "RangeRelationship" must {

    "convert to query" in {
      toQuery(RangeRelationship(3, 10)) shouldBe """[a1*3..10]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(RangeRelationship(3, 10)) shouldBe """[*3..10]"""
    }

    "convert to query with kinds" in {
      toQuery(RangeRelationship(3, 10, Label1, Label2)) shouldBe s"""[a1:$Label1|$Label2*3..10]"""
    }

  }

  "AnyLengthRelationship" must {

    "convert to query" in {
      toQuery(AnyLengthRelationship()) shouldBe """[a1*]"""
    }

    "convert to query without an identifier" in {
      toQueryEmpty(AnyLengthRelationship()) shouldBe """[*]"""
    }

  }


}
