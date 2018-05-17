package expression

import value._
import context._
/*
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression
{
  def execute(env: Environment): Value = {
    //val args: List[Value] = operands.map(_.execute(env)) //eager execution
    val args: List[Value] = if(Flags.parameterPassing == Flags.passByText)
      operands.map(Text(_))
    else if(Flags.parameterPassing == Flags.passByName)
      operands.map(x => new Thunk(x, env))
    else
      operands.map(_.execute(env)) //eager execution
    //alu.execute(operator, args)
    try
    {
      val maybeClosure = operator.execute(env)
      if(!maybeClosure.isInstanceOf[Closure]) throw new TypeException("undefined")
      else maybeClosure.asInstanceOf[Closure](args, env)
    }
    catch
    {
      //case e: UndefinedException => alu.execute(operator, args)
      case e: UndefinedException => alu.execute(operator, operands.map(_.execute(env)))
      //case e: TypeException => alu.execute(operator, args)
    }
  }
}
*/

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {


  def execute(env: Environment): Value = {

    try {
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) {
        val closure = maybeClosure.asInstanceOf[Closure]
        val args = Flags.parameterPassing match {
          case Flags.passByValue => operands.map(_.execute(env))
          case Flags.passByText => operands.map(new Text(_))
          case Flags.passByName => operands.map(new Thunk(_, env))
        }
        closure(args, env)
      } else {
        throw new TypeException("Only functions can be called")
      }
    } catch {
      case e: UndefinedException =>
        val args = operands.map(_.execute(env))
        alu.execute(operator, args)
    }
  }
}