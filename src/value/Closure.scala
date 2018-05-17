package value

import expression._
import context._
/*
class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value
{
  def apply(args: List[Value], callingEnv: Environment = null) = 
  {
    val tempEnv = if(!Flags.useStaticScopeRule && callingEnv != null) new Environment(callingEnv) else new Environment(defEnv)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}
*/

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
   def apply(args: List[Value], callEnv: Environment = null): Value = {
     val localEnv =
       if (Flags.useStaticScopeRule) new Environment(defEnv)
       else new Environment(callEnv)
     localEnv.bulkPut(params, args)
     body.execute(localEnv)
   }
}