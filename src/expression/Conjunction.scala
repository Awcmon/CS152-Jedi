package expression

import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm 
{
  def execute(env: Environment) = if(operands.head == null) Boole(true) 
    else if(operands.head.execute(env) == Boole(false)) Boole(false) 
    else Conjunction(operands.tail).execute(env)
}