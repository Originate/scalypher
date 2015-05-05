# Scalypher [![Circle CI](https://circleci.com/gh/Originate/scalypher/tree/master.svg?style=svg)](https://circleci.com/gh/Originate/scalypher/tree/master)

A DSL for building Neo4j Cypher queries in Scala. This project is in pre alpha, and is not ready for production use.

## Usage

To use Scalypher in an SBT project, you can use the `dependsOn` method in `build.sbt`:


```scala
val ScalypherVersion = "0.0.1"

lazy val root: Project = Project("root", file(".")).dependsOn(
  ProjectRef(uri("git://github.com/Originate/scalypher.git#v" + ScalypherVersion), "scalypher")
)
```

## Test

Clone the repo, then:

```bash
sbt
test
```

## Contribute

Contributions are welcome... submit pull requests!
