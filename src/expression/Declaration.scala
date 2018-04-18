package expression

import context._
import value._

case class Declaration(val id: Identifier, val exp: Expression) extends Expression
{
  def execute(env: Environment) = {env(id) = exp.execute(env); Notification.DONE;}
}
