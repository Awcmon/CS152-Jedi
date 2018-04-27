package expression

import context._
import value._

case class Block(val expressions: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = 
    {
      val tempEnv = new Environment(env)
      if(expressions.tail == null) expressions.head.execute(tempEnv) else Block(expressions.tail).execute(tempEnv)
    }
}
