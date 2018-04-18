package expression

import value._
import context._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression
{
  def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}