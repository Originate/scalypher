package com.originate.scalypher.util

object Exceptions {

  class MismatchedInterpolatedStringWithReferences(string: String, contains: Int, references: Int)
    extends Exception(s"""Provided string "$string" has $contains references but $references were given""")

  class IdentifierDoesntExistException
    extends Exception("Identifier not found for identifiable node/path/relationship")

  class IdentifierAliasCollisionException(name: String)
    extends Exception(s"Alias $name cannot be used because it collides with an object's name in your path, consider using a different name")

}
