package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
   def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
     case id ~ "=" ~ exp => Assignment(id, exp)
   }
  
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
     case "while"~"("~cond~")"~exp => Iteration(cond, exp)
   }
  
  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ expression ~ "]" => FunCall(Identifier("dereference"), List(expression))
  }
  
  //LOOP ::= "loop"~"["~EXPRESSION~"]"~EXPRESSION
  def loop: Parser[Loop] = "loop"~"["~expression~"]"~expression ^^ {
    case "loop"~"["~iters~"]"~body => Loop(iters, body)
  }

  override def expression: Parser[Expression] = loop | declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}
