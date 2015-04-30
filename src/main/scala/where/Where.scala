package com.scalypher.where

import com.scalypher.types.ReferenceableMap
import com.scalypher.types.Referenceable

case class AndOrCondition(operator: BooleanOperator, condition: Condition) {
  def toQuery(referenceableMap: ReferenceableMap): String =
    Seq(operator.toQuery, condition.toQuery(referenceableMap)) mkString " "
}

case class Where(startCondition: Condition, conditions: Seq[AndOrCondition] = Seq.empty) {

  val referenceables: Set[Referenceable] = {
    val cs = (conditions map (_.condition)) :+ startCondition
    cs.flatMap(_.referenceables).toSet
  }

  def and(condition: Condition): Where =
    copy(conditions = conditions :+ AndOrCondition(And, condition))

  def or(condition: Condition): Where =
    copy(conditions = conditions :+ AndOrCondition(Or, condition))

  def toQuery(referenceableMap: ReferenceableMap): String = {
    val firstCondition = startCondition.toQuery(referenceableMap)
    val rest =
      if (conditions.isEmpty) ""
      else " " + (conditions map (_.toQuery(referenceableMap)) mkString " ")

    s"$firstCondition$rest"
  }

}
