package expression

import context._
import value._

case class Block(val expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = if(expressions.tail == null) expressions.head.execute(env) else Block(expressions.tail).execute(env)
}
