package expression

import context._
import value._

case class Identifier(val name: String) extends Expression {
  def execute(env: Environment) = if(env(this).isInstanceOf[Text]) env(this).asInstanceOf[Text].body.execute(env) else env(this)
  override def toString = name
    
}