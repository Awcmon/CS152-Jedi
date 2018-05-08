package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
   def assignment: Parser[Expression] = identifier ~ "=" ~ expression ^^ {
     case id ~ "=" ~ exp => ???
   }
  
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Conditional] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
     case "while"~"("~cond~")"~exp => ???
   }
  
  // dereference ::= "[" ~ expression ~ "]"
  

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}
