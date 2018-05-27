// List implementation from week 3
trait List[T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  override def isEmpty: Boolean = false

}

class Nil[T] extends List[T] {

  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")

}

// create a List in the form List(), List(1), List(1,2)
object List {

  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))

  def apply[T](x: T): List[T] = new Cons(x, new Nil)

  def apply[T](): List[T] = new Nil

}

List()
List(1)
List(1, 2)
