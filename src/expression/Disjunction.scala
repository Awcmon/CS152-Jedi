package expression

import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm 
{
    def execute(env: Environment) = if(operands.head == null) Boole(false) 
    else if(operands.head.execute(env) == Boole(true)) Boole(true) 
    else Disjunction(operands.tail).execute(env)
}