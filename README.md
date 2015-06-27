# Scalypher [![Circle CI](https://circleci.com/gh/Originate/scalypher/tree/master.svg?style=svg)](https://circleci.com/gh/Originate/scalypher/tree/master) [![Coverage Status](https://coveralls.io/repos/Originate/scalypher/badge.svg?branch=master)](https://coveralls.io/r/Originate/scalypher?branch=master)

A DSL for building Neo4j Cypher queries in Scala. This project is in pre alpha, and is not ready for production use.

## Adding To Your Project

To use Scalypher in an SBT project, you can use the `dependsOn` method in `build.sbt`:

```scala
val ScalypherVersion = "0.0.1"

lazy val root: Project = Project("root", file(".")).dependsOn(
  ProjectRef(uri("git://github.com/Originate/scalypher.git#v" + ScalypherVersion), "scalypher")
)
```

## Getting Started

Scalypher is designed to look similar to Cypher queries:

```scala
val startNode = AnyNode()

val cypher = startNode --> AnyNode() where (
		startNode.property("thing") <> "something"
	) returns startNode

cypher.toQuery
// returns: 'MATCH (a1)-->() WHERE a1.thing <> "something" RETURN a1'
```

The main thing to note is that, if you would like to specify a node or relationship in a `WHERE` or
`RETURN` expression, you need to retain a reference to it and use that when building your Cypher expressions. This
eliminates the need to manually choose identifiers for your nodes/relationships/paths.

### Referencing The Path

If you'd like to reference the path from your `MATCH` expression in your `WHERE` expression, you can do

```scala
val cypher = startNode --> AnyNode() where { path =>
		...
	} returns startNode

```

### Getting Return Columns

When building out your persistence layer, it is likely you will need to know the identifier used in your
RETURN expressions. This can be obtained with:

```scala
query.getReturnColumns
```

Note that when your query's action is `DELETE`, there are no return columns, and when it is `RETURN *`,
you'll get back an identifier for each node, relationship, and one identifier for the path.

### Passing In Custom Types

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
		startNode.property("thing") === Instant.now
	) returns startNode

```

Note that the `CypherExpressible` trait provides the helper methods `safeWrapString` and `wrapString` to inject
string literals into your Cypher query (`safeWrapString` escapes quotes).

## Test

Clone the repo, then:

```bash
sbt
test
```

## Contribute

Contributions are welcome... submit pull requests!
