package com.originate.scalypher.action

import com.originate.scalypher.path.Path
import com.originate.scalypher.ToQueryWithIdentifiers
import com.originate.scalypher.types.Referenceable
import com.originate.scalypher.types.Identifiable
import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.util.Exceptions.IdentifierAliasCollisionException
import com.originate.scalypher.util.Exceptions.IdentifierDoesntExistException
import com.originate.scalypher.where.ReferenceableReference
import com.originate.scalypher.where.ObjectReference

sealed trait ActionItem extends ToQueryWithIdentifiers {

  val as: Option[String]

  def getReferenceable: Option[Identifiable]

  def as(name: String): ActionItem

  def toColumn(identifiableMap: IdentifiableMap): String

  protected def asString(identifiableMap: IdentifiableMap): Option[String] =
    as map { name =>
      if (identifiableMap.values.toSet contains name)
        throw new IdentifierAliasCollisionException(name)
      s"AS $name"
    }

}

case class ActionPath(
  path: Path,
  as: Option[String] = None
) extends ActionItem {

  val getReferenceable = Some(path)

  def as(name: String): ActionPath =
    copy(as = Some(name))

  def toQuery(identifiableMap: IdentifiableMap): String =
    identifiableMap get path map { identifier =>
      Seq(Some(identifier), asString(identifiableMap)).flatten mkString " "
    } getOrElse (throw new IdentifierDoesntExistException())

  def toColumn(identifiableMap: IdentifiableMap): String =
    as orElse identifiableMap.get(path) getOrElse (throw new IdentifierDoesntExistException())

}

case class ActionReference(
  reference: ReferenceableReference,
  as: Option[String] = None
) extends ActionItem {

  val getReferenceable = reference.getReferenceable

  def as(name: String): ActionReference =
    copy(as = Some(name))

  def toQuery(identifiableMap: IdentifiableMap): String =
    Seq(Some(reference.toQuery(identifiableMap)), asString(identifiableMap)).flatten mkString " "

  def toColumn(identifiableMap: IdentifiableMap): String =
    as getOrElse reference.toQuery(identifiableMap)

}
