package value

import expression._
import context._

trait Value
class Notification() extends Value {
  val message = "notification"
  override def toString = message
}
//honestly not sure what exactly you want for part 6
object Notification {
  def apply() = toString
  object OK extends Notification{
    override val message = "ok"
  }
  object DONE extends Notification{
    override val message = "done"
  }
  object UNSPECIFIED extends Notification{
    override val message = "unspecified"
  }
}

case class Variable() extends Value
case class Store() extends Value
case class Closure() extends Value
/*
trait Literal extends Value with Expression
{
  def execute(env: Environment) = this match {
  		case Boole(value) => Boole(value)
  		case Chars(value) => Chars(value)
  		case Integer(value) => Integer(value)
  		case Real(value) => Real(value)
  		case _ => throw new Exception("Unrecognized expression")
  	}
}
*/

case class Boole(val value: Boolean) extends Literal
{
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)
  override def toString = value.toString
}

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals 
{
  def +(other: Chars) = Chars(this.value + other.value)
  def ==(other: Chars) = Boole(this.value.equals(other.value))
  def substring(n1: Integer, n2: Integer) = Chars(value.substring(n1.value, n2.value))
  override def toString = value.toString
  def compare(other: Chars): Int = value.compareTo(other.value)
  override def canEqual(other: Any) =  other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean = other match {
    case other: Chars => this.canEqual(other) && (other.value.equals(this.value))
    case _ => false
  }
  override def hashCode = this.toString.##
  //def execute(env: Environment) = Chars(value)
}

case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals 
{
  def +(other: Integer) = Integer(this.value + other.value)
  def *(other: Integer) = Integer(this.value * other.value)
  def -(other: Integer) = Integer(this.value - other.value)
  def /(other: Integer) = if(other.value == 0) throw new Exception("Divide by zero") else Integer(this.value / other.value)
  def unary_- = Integer(-value) // unary negation
  override def toString = value.toString
  def compare(other: Integer): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean = other match {
    case other: Integer => this.canEqual(other) && (other.value == this.value)
    case _ => false
  }
  override def hashCode = this.toString.##
  //def execute(env: Environment) = Integer(value)
}
object Integer {
  implicit def intToReal(n: Integer): Real = Real(n.value.toDouble)
}

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {
  def +(other: Real) = Real(this.value + other.value)
  def *(other: Real) = Real(this.value * other.value)
  def -(other: Real) = Real(this.value - other.value)
  def /(other: Real) = if(other.value == 0.0) throw new Exception("Divide by zero") else Real(this.value / other.value)
  def unary_- = Real(-value) // unary negation
  override def toString = value.toString
  def compare(other: Real): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Real]
  override def equals(other: Any): Boolean = other match {
    case other: Real => this.canEqual(other) && (other.value == this.value)
    case _ => false
  }
  override def hashCode = this.toString.##
  //def execute(env: Environment) = Real(value)
}
/*
object Real {
  implicit def realToInt(n: Real): Integer = Integer(n.value.toInt)
}
*/


