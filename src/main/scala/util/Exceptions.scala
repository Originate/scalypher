package com.scalypher.util

object Exceptions {

  class MismatchedInterpolatedStringWithReferences(string: String, contains: Int, references: Int)
    extends Exception(s"""Provided string "$string" has $contains references but $references were given""")

  class IdentifierDoesntExistException
    extends Exception("Identifier not found for referenceable node/path/relationship")

}
