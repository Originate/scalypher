# Scalypher [![Circle CI](https://circleci.com/gh/Originate/scalypher/tree/master.svg?style=svg)](https://circleci.com/gh/Originate/scalypher/tree/master) [![Coverage Status](https://coveralls.io/repos/Originate/scalypher/badge.svg?branch=master)](https://coveralls.io/r/Originate/scalypher?branch=master)

A DSL for building Neo4j Cypher queries in Scala. This library is in use in production but does
not yet cover the *full* spec of the Cypher query language.

## Adding To Your Project

To use Scalypher in an SBT project, you can use the `dependsOn` method in `build.sbt`:

```scala
val ScalypherVersion = "0.0.9"

lazy val root: Project = Project("root", file(".")).dependsOn(
  ProjectRef(uri("git://github.com/Originate/scalypher.git#v" + ScalypherVersion), "scalypher")
)
```

## Getting Started

Scalypher is designed to look similar to Cypher queries:

```scala
val startNode = AnyNode()

val cypher = startNode --> AnyNode() where (
    startNode.property("name") <> "matt"
  ) returns startNode

cypher.toQuery
// returns: 'MATCH (a1)-->() WHERE a1.name <> "matt" RETURN a1'
```

The main thing to note is that, if you would like to specify a node or relationship in a `WHERE` or
`RETURN` expression, you need to retain a reference to it and use that when building your Cypher expressions. This
eliminates the need to manually choose identifiers for your nodes/relationships/paths.

### Referencing The Path

If you'd like to reference the path from your match expression in your where expression, pass a
`Path => Where` to the `where` method

```scala
val cypher = startNode --> AnyNode() where { path =>
    ...
  } returns startNode
```

### Getting Return Columns

When building out your persistence layer, it is likely you will need to know the identifier used in your
return expressions. This can be obtained with:

```scala
cypher.getReturnColumns
```

Note that when your query's action is `DELETE`, there are no return columns, and when it is `RETURN *`,
you'll get back an identifier for each node, relationship, and one identifier for the path.

#### Aliasing

Objects passed in your return clauses can be aliased to any name you want. For example

```scala
val cypher = startNode --> AnyNode() where (
    startNode.property("name") <> "matt"
  ) returns (startNode as "startNode", startNode.property("name") as "name")

cypher.getReturnColumnes
// returns: Set("startNode", "name")
```

### Properties

Properties of a node/relationship can be checked against other properties or values

```scala
val cypher = startNode where (startNode.property("name") === "matt") returns startNode

cypher.toQuery
// returns: 'MATCH (a1) WHERE a1.name = "matt" RETURN a1'
```

## Where Clauses

Simple conditions in Scalypher can be created using references to nodes, relationships, and paths combined
with the operators found in Cypher (with the exception of *equals*, which uses `===`).

```scala
val node = AnyNode()

node.property("name") === "matt"
node.property("name") <> "matt"
node.property("age") > 12
node.property("age") < 12
node.property("age") >= 12
node.property("age") <= 12
node.property("name") in Seq("matt", "andy")
```

Conditions can be chained together with `and` and `or`

```scala
val node = AnyNode()

(node.property("name") === "matt") and
  (node.property("age") > 12)
```

### Predicates

Predicates are used to assert conditions on a collection of elements. You can create predicate
conditions in your query like this

```scala
import com.originate.scalypher.where.All

val cypher = startNode -> AnyNode() where { path =>
    All nodesIn path where { node =>
      node.property("name") <> "matt"
    }
  } returns startNode

cypher.toQuery
// returns: MATCH a2 = (a1)-->() WHERE ALL (x IN NODES(a2) WHERE x.name <> "matt") RETURN a1
```

Note that you can use the singular version as well `Any nodeIn path where { node => ... }`

### Custom Expressions

Since Scalypher doesn't cover the complete language, we have added a way to build custom expressions
while maintaining the appropriate node/relationship/path identifiers.

```scala
import com.originate.scalypher.where.Expression

val cypher = startNode -> endNode where (
    Expression("id(?) <> ?", endNode, 10)
  ) returns startNode

cypher.toQuery
// returns: MATCH (a1)-->(a2) WHERE id(a2) <> 10 RETURN a1
```

## Using Custom Types In Query Expressions

Scalypher uses a `CypherExpressible` typeclass in order to allow extending the DSL to handle any type you
want to pass to it. Here's an example of how you could use `org.joda.time.Instant` as a value reference
in your project.

```scala
import org.joda.time.Instant
import com.originate.scalypher.CypherExpressible

object CypherExpressibles {

  implicit object CypherExpressibleInstant extends CypherExpressible[Instant] {
    def toQuery(instant: Instant): String =
      wrapString(instant.toString)
  }

}
```

This can then be used in your code:

```scala
import CypherExpressibles._

val cypher = startNode --> AnyNode() where (
    startNode.property("createdAt") === Instant.now
  ) returns startNode

```

Note that the `CypherExpressible` trait provides the helper methods `safeWrapString` and `wrapString` to inject
string literals into your Cypher query (`safeWrapString` escapes quotes).

## Test

Clone the repo, then:

```bash
sbt test
```

## Contribute

Contributions are welcome... submit pull requests!
