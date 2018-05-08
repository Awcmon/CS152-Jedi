package expression

import context._
import value._

case class Iteration(val cond: Expression, val body: Expression) extends SpecialForm 
{
  def execute(env: Environment) = {while(cond.execute(env) == Boole(true)) body.execute(env); Notification.DONE}
}
