package test

import com.scalypher.where.Comparison
import com.scalypher.where.NotEqual
import com.scalypher.where.Equal
import com.scalypher.where.ValueReference
import com.scalypher.path.AnyNode
import com.scalypher.action.ReturnDistinct
import com.scalypher.Query

import org.scalatest._

class QuerySpec extends WordSpec with Matchers {

  val startNode = AnyNode()
  val returns = ReturnDistinct(startNode)

  "building a full query" must {

    "allow simple syntax for simple queries" in {
      val query = startNode --> AnyNode() where (startNode.property("thing") <> "something") returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" RETURN a1"""
    }

    "allow simple syntax for more complex queries" in {
      val query = startNode --> AnyNode() where (
        (startNode.property("thing") <> "something") and
        (startNode.property("otherThing") == "something")
      ) returns startNode
      query.toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" AND a1.otherThing = "something" RETURN a1"""
    }

  }

  "a where clause is given" must {

    val where = Comparison(startNode.property("thing"), NotEqual, ValueReference("something"))

    "space out the match, where, and return clauses" in {
      val path = startNode --> AnyNode()
      Query(path, where, returns).toQuery shouldBe """MATCH (a1)-->() WHERE a1.thing <> "something" RETURN DISTINCT a1"""
    }

  }

  "no where clause is given" must {

    "space out the match and return clauses" in {
      val path = startNode --> AnyNode()
      Query(path, returns).toQuery shouldBe "MATCH (a1)-->() RETURN DISTINCT a1"
    }

  }

  "given a return expression that references something not in the where clause" must {

    val node2 = AnyNode()
    val path = startNode -- node2
    val where = Comparison(startNode.property("thing"), Equal, ValueReference("something"))
    val query = Query(path, where, ReturnDistinct(node2))

    "not throw an exception" in {
      noException should be thrownBy {
        query.getReturnColumn
      }
    }

    "give the identifier for the return node" in {
      val identifier = query.getIdentifier(node2).get
      identifier shouldBe query.getReturnColumn
    }

  }

}
