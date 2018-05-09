package expression

import context._
import value._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm 
{
  def execute(env: Environment) = { env(vbl).asInstanceOf[Variable].content = update.execute(env); Notification.DONE }
}
