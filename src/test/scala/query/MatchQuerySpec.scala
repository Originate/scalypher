package test.query

import com.originate.scalypher.where.Comparison
import com.originate.scalypher.where.NotEqual
import com.originate.scalypher.where.Equal
import com.originate.scalypher.where.ValueReference
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.MatchQuery

import org.scalatest._

class MatchQuerySpec extends WordSpec with Matchers {

  val startNode = AnyNode()

  val returns = ReturnDistinct(startNode)

  "building a full query" must {

    "allow a super simple node lookup" in {
      (startNode returns startNode).toQuery shouldBe """MATCH (a1) RETURN a1"""
    }

    "allow pathless simple syntax for simple queries" in {
      val query = startNode where (startNode.property("thing") === "something") returns startNode
      query.toQuery shouldBe """MATCH (a1) WHERE a1.thing = "something" RETURN a1"""
    }

    "allow simple syntax for simple queries" in {
      val query = startNode --> AnyNode() where (startNode.property("thing") <> "something") returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" RETURN a1"""
    }

    "allow simple syntax for more complex queries" in {
      val query = startNode --> AnyNode() where (
        (startNode.property("thing") <> "something") and
        (startNode.property("otherThing") === "something")
      ) returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" AND a1.otherThing = "something" RETURN a1"""
    }

  }

  "a where clause is given" must {

    val where = Comparison(startNode.property("thing"), NotEqual, ValueReference("something"))

    "space out the match, where, and return clauses" in {
      val path = startNode --> AnyNode()
      MatchQuery(path, where, returns).toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" RETURN DISTINCT a1"""
    }

  }

  "no where clause is given" must {

    "space out the match and return clauses" in {
      val path = startNode --> AnyNode()
      MatchQuery(path, returns).toQuery shouldBe "MATCH (a1)-->() RETURN DISTINCT a1"
    }

  }

  "given a return expression that references something not in the where clause" must {

    val node2 = AnyNode()
    val path = startNode -- node2
    val where = Comparison(startNode.property("thing"), Equal, ValueReference("something"))
    val query = MatchQuery(path, where, ReturnDistinct(node2))

    "not throw an exception" in {
      noException should be thrownBy {
        query.getReturnColumns
      }
    }

    "give the identifier for the return node" in {
      val identifier = query.getIdentifier(node2).get
      Set(identifier) shouldBe query.getReturnColumns
    }

  }

  "getting return columns" when {

    "given a return all expression" must {

      "return columns for as many referenceables there are in the match expression" in {
        val query = (AnyNode() -- AnyRelationship() -- AnyNode()).returnAll
        query.getReturnColumns.size shouldBe 4
      }

    }

    "given a return expression" must {

      val endNode = AnyNode()

      "can return one column" in {
        val query = startNode -- AnyRelationship() -- endNode returns startNode
        query.getReturnColumns.size shouldBe 1
      }

      "can return multiple columns" in {
        val query = startNode -- AnyRelationship() -- endNode returns (startNode, endNode)
        query.getReturnColumns.size shouldBe 2
      }

    }

    "given a delete expression" must {

      "have no return columns" in {
        val startNode = AnyNode()
        val query = startNode -- AnyRelationship() -- AnyNode() delete startNode
        query.getReturnColumns.size shouldBe 0
      }

    }

  }

}
