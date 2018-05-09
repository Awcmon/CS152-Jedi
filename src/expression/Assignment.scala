package expression

import context._
import value._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm 
{
  def execute(env: Environment) = ???
}