package test.query

import com.originate.scalypher.path.AnyNode
import com.originate.scalypher.path.CypherNode
import com.originate.scalypher.path.KindRelationship
import com.originate.scalypher.MergeQuery

import org.scalatest._

class MergeQuerySpec extends WordSpec with Matchers {

  val startNode = CypherNode("label1")
  val createRelationship = KindRelationship("label")
  val endNode = CypherNode("label2")
  val matchPath = startNode -- endNode
  val createPath = startNode -- createRelationship -- endNode
  val where = startNode.property("id") <> "test"

  "only a merge path is given" must {

    "produce a query that merges a node" in {
      val create = MergeQuery(startNode)
      create.toQuery shouldBe "MERGE (:label1)"
    }

  }

  "a match and merge path is given" must {

    "use simple identifiers for references in the merge path" in {
      val create = matchPath merge createPath
      create.toQuery shouldBe "MATCH (a1:label1)--(a2:label2) MERGE (a1)-[:label]-(a2)"
    }

    "include a return statement if provided" in {
      val create = matchPath merge createPath returnDistinct startNode
      create.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) MERGE (a1)-[:label]-(a2) RETURN DISTINCT a1"""
    }

    "add identifier to reference if it appears in the return statement" in {
      val create = matchPath merge createPath returnDistinct createRelationship
      val query = create.toQuery
      query should include (")-[a1:label]-(")
      query should endWith ("RETURN DISTINCT a1")
    }

    "allow multiple match paths and" must {

      val otherNode = AnyNode()
      val matchPath2 = startNode --> otherNode
      val createPath = startNode -- createRelationship -- endNode --> otherNode

      "assign identifiers to the necessary elements" in {
        val merge = MergeQuery(createPath, Seq(matchPath, matchPath2))
        val query = merge.toQuery
        query should startWith regex """MATCH \(a\d:label1\)--\(a\d:label2\), \(a\d:label1\)-->\(a\d\)"""
        query should endWith regex """MERGE \(a\d\)-\[:label]-\(a\d\)-->\(a\d\)"""
      }

      "assign identifiers appropriately" when {

        val merge = MergeQuery(createPath, Seq(matchPath, matchPath2))
          .onCreate(Seq(startNode.property("a").set("b")))
          .onMerge(Seq(createRelationship.property("c").set("d"), otherNode.property("e").set("f")))

        def extractIds(query: String) = {
          val Identifiers = """MATCH \((\w+):label1\)--\((\w+):label2\), \((\w+):label1\)-->\((\w+)\).*""".r
          val CreateIdentifier = """.*\[(\w*):label\].*""".r

          query match {
            case Identifiers(startId, endId, startId2, otherId) if startId == startId2 =>
              query match {
                case CreateIdentifier(relationshipId) => (startId, endId, otherId, relationshipId)
              }
          }
        }

        "created node/relationship is not referenced" in {
          val query = (merge onMerge Seq()).toQuery
          val (startId, endId, otherId, relationshipId) = extractIds(query)

          val mergeTest = s"""MERGE ($startId)-[$relationshipId:label]-($endId)-->($otherId)"""
          val onCreateTest = s"""ON CREATE SET $startId.a = "b""""
          query should include (mergeTest)
          query should include (onCreateTest)
          query should not include ("""ON MERGE SET""")
          relationshipId shouldBe ""
        }

        "multiple properties are set" in {
          val query = merge.toQuery
          val (startId, endId, otherId, relationshipId) = extractIds(query)

          val mergeTest = s"""MERGE ($startId)-[$relationshipId:label]-($endId)-->($otherId)"""
          val onCreateTest = s"""ON CREATE SET $startId.a = "b""""
          val onMergeTest = s"""ON MERGE SET $relationshipId.c = "d", $otherId.e = "f""""
          query should include (onMergeTest)
        }

        "multiple properties are set and a return clause is given" in {
          val query = (merge returns createRelationship).toQuery
          val (startId, endId, otherId, relationshipId) = extractIds(query)

          val mergeTest = s"""MERGE ($startId)-[$relationshipId:label]-($endId)-->($otherId)"""
          val onCreateTest = s"""ON CREATE SET $startId.a = "b""""
          val onMergeTest = s"""ON MERGE SET $relationshipId.c = "d", $otherId.e = "f""""
          val returnTest = s"""RETURN $relationshipId"""
          query should include (mergeTest)
          query should include (onCreateTest)
          query should include (onMergeTest)
          query should endWith (returnTest)
        }

      }

    }

  }

  "multiple nodes/relationships are updated on create/merge" must {

    val mergeQuery = matchPath merge createPath

    "reference the appropriate nodes via identifiers" in {
      val query = mergeQuery onCreate Seq(startNode.property("a").set("b"), createRelationship.property("b").set("c"))
      query.toQuery shouldBe """MATCH (a1:label1)--(a2:label2) MERGE (a1)-[a3:label]-(a2) ON CREATE SET a1.a = "b", a3.b = "c""""
    }

  }

  "on create properties are provided" must {

    "include ON CREATE SET clause" in {
      val mergeQuery = MergeQuery(startNode) onCreate Seq(startNode.property("a").set("b"))
      mergeQuery.toQuery shouldBe """MERGE (a1:label1) ON CREATE SET a1.a = "b""""
    }

    "allow multiple properties to be set" in {
      val mergeQuery = MergeQuery(startNode) onCreate Seq(startNode.property("a").set("b"), startNode.property("b").set("c"))
      mergeQuery.toQuery shouldBe """MERGE (a1:label1) ON CREATE SET a1.a = "b", a1.b = "c""""
    }

  }

  "on merge properties are provided" must {

    "include ON MERGE SET clause" in {
      val mergeQuery = MergeQuery(startNode) onMerge Seq(startNode.property("a").set("b"))
      mergeQuery.toQuery shouldBe """MERGE (a1:label1) ON MERGE SET a1.a = "b""""
    }

    "allow multiple properties to be set" in {
      val mergeQuery = MergeQuery(startNode) onMerge Seq(startNode.property("a").set("b"), startNode.property("b").set("c"))
      mergeQuery.toQuery shouldBe """MERGE (a1:label1) ON MERGE SET a1.a = "b", a1.b = "c""""
    }

  }

}
