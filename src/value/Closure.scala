package value

import expression._
import context._

case class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value
{
  def apply(args: List[Value], callingEnv: Environment = null) = 
  {
    var tempEnv = new Environment(defEnv)
    if(!Flags.useStaticScopeRule && callingEnv != null)
    {
      tempEnv = new Environment(callingEnv)
    }
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}
