package value

import expression._
import context._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value
{
  def apply(args: List[Value]): Value = 
  {
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}
