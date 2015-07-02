package com.originate.scalypher.test.action

import com.originate.scalypher.action.ActionReference
import com.originate.scalypher.action.ActionPath
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException
import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException

import org.scalatest._

class ActionItemSpec extends WordSpec with Matchers {

  val node = AnyNode()
  val nodeName = "a1"
  val pathName = "a2"
  val path = node --> AnyNode()
  val map: IdentifiableMap = Map(node -> nodeName, path -> pathName)

  "action items" must {

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

  "action paths" must {

    val actionPath = ActionPath(path)

    "add aliases inline" in {
      (path as "path").toQuery(map) shouldBe s"$pathName AS path"
    }

    "provide 'as' as the column" in {
      actionPath.as("path").toColumn(map) shouldBe "path"
    }

    "provide identifier as the column" in {
      actionPath.toColumn(map) shouldBe pathName
    }

    "throw an exception if neither an 'as' or identifier are found" in {
      an [IdentifierDoesntExistException] should be thrownBy actionPath.toColumn(Map.empty)
    }

  }

}
