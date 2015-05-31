package test.query

import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.CreateQuery

import org.scalatest._

class CreateQuerySpec extends WordSpec with Matchers {

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

      val query = create.toQuery
      query should startWith regex """MATCH \(a\d:label1\)--\(a\d:label2\), \(a\d:label1\)-->\(a\d\)"""
      query should endWith regex """CREATE \(a\d\)-\[:label]-\(a\d\)-->\(a\d\)"""
    }

    "include a where path if provided" in {
      val create = matchPath where where create createPath
      create.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) WHERE a1.id <> "test" CREATE (a1)-[:label]-(a2)"""
    }

    "include a return statement if provided" in {
      val create = matchPath create createPath returnDistinct startNode
      create.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) CREATE (a1)-[:label]-(a2) RETURN DISTINCT a1"""
    }

    "add identifier to reference if it appears in the return statement" in {
      val create = matchPath create createPath returnDistinct createRelationship
      val query = create.toQuery
      query should include (")-[a1:label]-(")
      query should endWith ("RETURN DISTINCT a1")
    }

  }

}
