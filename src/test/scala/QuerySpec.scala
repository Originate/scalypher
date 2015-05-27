package test

import com.originate.scalypher.where.Comparison
import com.originate.scalypher.where.NotEqual
import com.originate.scalypher.where.Equal
import com.originate.scalypher.where.ValueReference
import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.AnyRelationship
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.action.ReturnDistinct
import com.originate.scalypher.CreateQuery
import com.originate.scalypher.MatchQuery
import com.originate.scalypher.Query

import org.scalatest._

class QuerySpec extends WordSpec with Matchers {

  val startNode = AnyNode()

  "CreateQuery" when {

    val startNode = CypherNode("label1")
    val createRelationship = KindRelationship("label")
    val endNode = CypherNode("label2")
    val matchPath = startNode -- endNode
    val createPath = startNode -- createRelationship -- endNode
    val where = startNode.property("id") <> "test"

    "only a create path is given" must {

      "produce a query that creates a node" in {
        val create = CreateQuery(startNode)
        create.toQuery shouldBe "CREATE (:label1)"
      }

    }

    "a match and create path is given" must {

      "use simple identifiers for references in the create path" in {
        val create = matchPath create createPath
        create.toQuery shouldBe "MATCH (a1:label1)--(a2:label2) CREATE (a1)-[:label]-(a2)"
      }

      "allow multiple match paths" in {
        val otherNode = AnyNode()
        val matchPath2 = startNode --> otherNode
        val createPath = startNode -- createRelationship -- endNode --> otherNode
        val create = CreateQuery(createPath, Seq(matchPath, matchPath2), None, None)
        create.toQuery should endWith regex """CREATE \(a\d\)-\[:label]-\(a\d\)-->\(a\d\)"""
      }

      "include a where path if provided" in {
        val create = matchPath where where create createPath
        create.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) WHERE a1.id <> "test" CREATE (a1)-[:label]-(a2)"""
      }

      "include a return statement if provided" in {
        val create = matchPath create createPath returnDistinct startNode
        create.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) CREATE (a1)-[:label]-(a2) RETURN DISTINCT a1"""
      }

    }

  }

  "MatchQuery" when {

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


}
