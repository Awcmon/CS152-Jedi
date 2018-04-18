package expression

import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm 
{
  def execute(env: Environment) = FunCall(Identifier("equals"), operands).execute(env)
}