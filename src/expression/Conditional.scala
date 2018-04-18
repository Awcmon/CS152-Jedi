package expression

import context._
import value._

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression) extends SpecialForm
{
  def execute(env: Environment) = if(condition.execute(env) == Boole(true)) consequent.execute(env) else alternative.execute(env)
}