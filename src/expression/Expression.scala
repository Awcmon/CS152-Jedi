package expression

import context._
import value._

trait Expression {
  def execute(env: Environment): Value
}

/**********************************/

trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}

/**********************************/

case class Identifier(val name: String) extends Expression {
  def execute(env: Environment) = env(this)
  override def toString = name
    
}

