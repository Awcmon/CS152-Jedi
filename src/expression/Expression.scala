package expression

import value._

class Environment extends collection.mutable.HashMap[Identifier, Value]

trait Expression
{
  def execute(env: Environment): Value
}

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) = env(this)
}
