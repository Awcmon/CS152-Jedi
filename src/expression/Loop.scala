package expression

import context._
import value._

case class Loop(iters: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment) = 
  {
    val n = if(iters.execute(env).isInstanceOf[Integer]) iters.execute(env).asInstanceOf[Integer].value else throw new TypeException("Value must be a non-negative integer.")
    var i = 0
    for(i <- 0 until n)
    {
      body.execute(env)
    }
    Notification.DONE
  }
}
