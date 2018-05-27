package lessons.week4

import lessons.week3.{Empty, IntSet, NonEmpty}
// Covariance, contravariance and invariance

// List implementation from week 3

// Make T covariance by prefixing +
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  // make possible the contravariance of T by
  // bounding the elem to be a supertype of T
  def prepend[U >: T](elem: U): List[U] = new Cons[U](elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

// create a List in the form List(), List(1), List(1,2)
object List {
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, Nil))
  def apply[T](x: T): List[T] = new Cons(x, Nil)
  def apply[T](): List[T] = Nil

  // returns a list of IntSet since Empty is a IntSet and
  // satisfies the bounding
  def f(xs: List[NonEmpty]): List[IntSet] = xs prepend Empty
}

//List()
//List(1)
//List(1, 2)

//val strings: List[String] = Nil //covariance
