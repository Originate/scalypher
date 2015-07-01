package com.originate.scalypher.where

import com.originate.scalypher.types.IdentifiableMap
import com.originate.scalypher.types.Identifiable

case class AndOrCondition(operator: BooleanOperator, condition: Condition) {
  def toQuery(identifiableMap: IdentifiableMap): String =
    Seq(operator.toQuery, condition.toQuery(identifiableMap)) mkString " "
}

case class Where(startCondition: Condition, conditions: Seq[AndOrCondition] = Seq.empty) {

  val identifiables: Set[Identifiable] = {
    val cs = (conditions map (_.condition)) :+ startCondition
    cs.flatMap(_.identifiables).toSet
  }

  def and(condition: Condition): Where =
    copy(conditions = conditions :+ AndOrCondition(And, condition))

  def or(condition: Condition): Where =
    copy(conditions = conditions :+ AndOrCondition(Or, condition))

  def toQuery(identifiableMap: IdentifiableMap): String = {
    val firstCondition = startCondition.toQuery(identifiableMap)
    val rest =
      if (conditions.isEmpty) ""
      else " " + (conditions map (_.toQuery(identifiableMap)) mkString " ")

    s"$firstCondition$rest"
  }

}
