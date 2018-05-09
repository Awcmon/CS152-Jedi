package expression

import value._
import context._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression
{
  def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env)) //eager execution
    //alu.execute(operator, args)
    try
    {
      val maybeClosure = operator.execute(env)
      if(!maybeClosure.isInstanceOf[Closure]) throw new TypeException("undefined")
      else maybeClosure.asInstanceOf[Closure](args, env)
    }
    catch
    {
      case e: UndefinedException => alu.execute(operator, args)
      //case e: TypeException => alu.execute(operator, args)
    }
  }
}
