// Polymorphism
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

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

/**
  * Gets the nth element from a list
  */
def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)