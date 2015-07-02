package com.originate.scalypher.test.where

import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.util.Exceptions.MismatchedInterpolatedStringWithReferences
import com.originate.scalypher.where._
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.types.IdentifiableMap

import org.scalatest._

class ExpressionSpec extends WordSpec with Matchers {

  val node1 = AnyNode()
  val node2 = AnyNode()
  val identifiableMap: IdentifiableMap = Map(node1 -> "a", node2 -> "b")

  "fail if not enough arguments are given" in {
    a [MismatchedInterpolatedStringWithReferences] should be thrownBy {
      Expression("? > ?", node1.property("thing")).toQuery(identifiableMap)
    }
  }

  "fail if too many arguments are given" in {
    a [MismatchedInterpolatedStringWithReferences] should be thrownBy {
      Expression("? > 1", node1.property("thing"), node2.property("thing")).toQuery(identifiableMap)
    }
  }

  "fail if identifier doesn't exist in identifiable map" in {
    an [IdentifierDoesntExistException] should be thrownBy {
      Expression("? > 1", node1.property("thing")).toQuery(Map.empty)
    }
  }

  "replace question marks with node references" in {
    Expression("? > 1", node1.property("thing")).toQuery(identifiableMap) shouldBe """(a.thing > 1)"""
  }

  "replace multiple question marks with multiple references" in {
    Expression(
      "? > 1 AND ? < 2",
      node1.property("thing"),
      node1.property("thang")
    ).toQuery(identifiableMap) shouldBe """(a.thing > 1 AND a.thang < 2)"""
  }

  "replace question marks with serialized value references" in {
    val expression = Expression("? > ?", node1.property("thing"), """string "with" quotes""").toQuery(identifiableMap)
    expression shouldBe """(a.thing > "string \"with\" quotes")"""
  }

  "gets identifiables" in {
    val expression = Expression("? > ?", node1.property("thing"), """string "with" quotes""")
    expression.identifiables shouldBe Set(node1)
  }

}
