package value

import collection.mutable._
import context._

class Store(private var elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value {
  // adds e to the end of store
  def add(e: Value) {elems += e}
  // inserts e at position pos in this
  def put(e: Value, pos: Integer) {???}
  // removes element at position pos from this
  def rem(pos: Integer) {???}
  // returns element at position pos in this
  def get(pos: Integer): Value = ???
  // returns true ie this contains e
  def contains(e: Value): Boole = ???
  // returns the size of this
  def size: Integer = ???
  // returns "{e0 e1 e2 ...}"
  override def toString = {???}
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {???}
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {???}
}
