package test.where

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.types.ReferenceableMap

import org.scalatest._

class ReferenceSpec extends WordSpec with Matchers {

  val node1 = AnyNode()
  val emptyMap: ReferenceableMap = Map.empty

  "given a string" must {

    "serialize" in {
      ValueReference("asdf").toQuery(emptyMap) shouldBe "\"asdf\""
    }

    "escape quotes" in {
      val string = """asdf "asdf" asdf"""
      ValueReference(string).toQuery(emptyMap) shouldBe """"asdf \"asdf\" asdf""""
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

    }

  }

}
