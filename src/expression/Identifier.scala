package expression

import context._
import value._

case class Identifier(val name: String) extends Expression {
  def execute(env: Environment) = env(this)
  override def toString = name
    
}