package test.where

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.types.ReferenceableMap

import org.scalatest._

class WhereSpec extends WordSpec with Matchers {

  "References" when {

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

  }

  "Where Clauses" when {

    "given an Expression" must {

      val node1 = AnyNode()
      val node2 = AnyNode()
      val referenceableMap: ReferenceableMap = Map(node1 -> "a", node2 -> "b")

      "fail if not enough arguments are given" in {
        a [MismatchedInterpolatedStringWithReferences] should be thrownBy {
          Expression("? > ?", node1.property("thing")).toQuery(referenceableMap)
        }
      }

      "fail if too many arguments are given" in {
        a [MismatchedInterpolatedStringWithReferences] should be thrownBy {
          Expression("? > 1", node1.property("thing"), node2.property("thing")).toQuery(referenceableMap)
        }
      }

      "fail if identifier doesn't exist in referenceable map" in {
        an [IdentifierDoesntExistException] should be thrownBy {
          Expression("? > 1", node1.property("thing")).toQuery(Map.empty)
        }
      }

      "replace question marks with node references" in {
        Expression("? > 1", node1.property("thing")).toQuery(referenceableMap) shouldBe """(a.thing > 1)"""
      }

      "replace multiple question marks with multiple references" in {
        Expression(
          "? > 1 AND ? < 2",
          node1.property("thing"),
          node1.property("thang")
        ).toQuery(referenceableMap) shouldBe """(a.thing > 1 AND a.thang < 2)"""
      }

      "replace question marks with serialized value references" in {
        val expression = Expression("? > ?", node1.property("thing"), """string "with" quotes""").toQuery(referenceableMap)
        expression shouldBe """(a.thing > "string \"with\" quotes")"""
      }

    }

    "given multiple AnyNodes" must {

      val node1 = AnyNode()
      val node2 = AnyNode()
      val node3 = AnyNode()

      "generate a referenceableMap with unique identifiers for each" in {
        val where = Comparison(node1.property("test"), Equal, node2.property("test")) and
          Comparison(node3.property("test"), NotEqual, node1.property("test"))

        where.referenceables.size shouldBe 3
      }

    }

    "given multiple Nodes that are equivalent but different objects" must {

      val node1 = CypherNode("a")
      val node2 = CypherNode("a")
      val node3 = CypherNode("a")

      "generate a referenceableMap with unique identifiers for each" in {
        val where = Comparison(node1.property("test"), Equal, node2.property("test")) and
          Comparison(node3.property("test"), NotEqual, node1.property("test"))

        where.referenceables.size shouldBe 3
      }

    }

    "given multiple Relationships that are equivalent but different objects" must {

      val relationship1 = KindRelationship("a")
      val relationship2 = KindRelationship("a")
      val relationship3 = KindRelationship("a")

      "generate a referenceableMap with unique identifiers for each" in {
        val where = Comparison(relationship1.property("label"), Equal, relationship2.property("label")) and
          Comparison(relationship2.property("label"), NotEqual, relationship3.property("label"))

        where.referenceables.size shouldBe 3
      }

    }

  }


}
