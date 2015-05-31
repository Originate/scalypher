package com.originate.scalypher.test.where

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.CypherExpressible._

import org.scalatest._

class ReferenceSpec extends WordSpec with Matchers {

  val node1 = AnyNode()
  val emptyMap: ReferenceableMap = Map.empty

  "SeqValueReference" when {

    "given a sequence of strings" must {

      "serialize" in {
        SeqValueReference(Seq("a", "b")).toQuery(emptyMap) shouldBe """["a", "b"]"""
      }

    }

  }

  "ValueReferences" when {

    "given a string" must {

      "serialize" in {
        ValueReference("asdf").toQuery(emptyMap) shouldBe "\"asdf\""
      }

      "escape quotes" in {
        val string = """asdf "asdf" asdf"""
        ValueReference(string).toQuery(emptyMap) shouldBe """"asdf \"asdf\" asdf""""
      }

    }

    "given a double" must {

      "serialize" in {
        ValueReference(2.01).toQuery(emptyMap) shouldBe "2.01"
      }

    }

    "given an int" must {

      "serialize" in {
        ValueReference(2).toQuery(emptyMap) shouldBe "2"
      }

    }

    "given a boolean" must {

      "serialize" in {
        ValueReference(true).toQuery(emptyMap) shouldBe "TRUE"
        ValueReference(false).toQuery(emptyMap) shouldBe "FALSE"

      }

    }

    "given an option" must {

      val x: Option[String] = Some("string")
      val y: Option[String] = None

      "serialize some value" in {
        ValueReference(x).toQuery(emptyMap) shouldBe """"string""""
      }

      "serialize no value" in {
        ValueReference(y).toQuery(emptyMap) shouldBe "NULL"
      }

    }

  }

  "making comparisons" when {

    "given a string" must {

      "allow simple syntax for ===" in {
        node1.property("a") === "b" shouldBe Comparison(node1.property("a"), Equal, ValueReference("b"))
      }

      "allow simple syntax for <>" in {
        node1.property("a") <> "b" shouldBe Comparison(node1.property("a"), NotEqual, ValueReference("b"))
      }

      "allow simple syntax for <" in {
        node1.property("a") < "b" shouldBe Comparison(node1.property("a"), LT, ValueReference("b"))
      }

      "allow simple syntax for >" in {
        node1.property("a") > "b" shouldBe Comparison(node1.property("a"), GT, ValueReference("b"))
      }

      "allow simple syntax for <=" in {
        node1.property("a") <= "b" shouldBe Comparison(node1.property("a"), LTE, ValueReference("b"))
      }

      "allow simple syntax for >=" in {
        node1.property("a") >= "b" shouldBe Comparison(node1.property("a"), GTE, ValueReference("b"))
      }

      "allow simple syntax for IN" in {
        node1.property("a") in Seq("b", "c") shouldBe Comparison(node1.property("a"), IN, SeqValueReference(Seq("b", "c")))
      }

    }

    "given a number" must {

      "allow simple syntax for ===" in {
        node1.property("a") === 2 shouldBe Comparison(node1.property("a"), Equal, ValueReference(2))
      }

      "allow simple syntax for <>" in {
        node1.property("a") <> 2 shouldBe Comparison(node1.property("a"), NotEqual, ValueReference(2))
      }

      "allow simple syntax for <" in {
        node1.property("a") < 2 shouldBe Comparison(node1.property("a"), LT, ValueReference(2))
      }

      "allow simple syntax for >" in {
        node1.property("a") > 2 shouldBe Comparison(node1.property("a"), GT, ValueReference(2))
      }

      "allow simple syntax for <=" in {
        node1.property("a") <= 2 shouldBe Comparison(node1.property("a"), LTE, ValueReference(2))
      }

      "allow simple syntax for >=" in {
        node1.property("a") >= 2 shouldBe Comparison(node1.property("a"), GTE, ValueReference(2))
      }

      "allow simple syntax for IN" in {
        node1.property("a") in Seq(1, 2) shouldBe Comparison(node1.property("a"), IN, SeqValueReference(Seq(1, 2)))
      }

    }

  }

}
