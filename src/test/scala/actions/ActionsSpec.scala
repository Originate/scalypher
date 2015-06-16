package com.originate.scalypher.test.action

import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.types.ReferenceableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException

import org.scalatest._

class ActionsSpec extends WordSpec with Matchers {

  val node = AnyNode()
  val nodeName = "a1"
  val map: ReferenceableMap = Map(node -> nodeName)

  "action references" must {

    "add aliases inline" in {
      (node as "something").toQuery(map) shouldBe s"$nodeName AS something"
    }

    "add aliases to references with properties" in {
      (node property "thing" as "otherThing").toQuery(map) shouldBe s"$nodeName.thing AS otherThing"
    }

    "allow no aliases" in {
      ActionReference(node).toQuery(map) shouldBe nodeName
    }

    "throw an exception when the alias collides" in {
      an [IdentifierAliasCollisionException] should be thrownBy (node as nodeName).toQuery(map)
    }

  }

}
