package lessons.week2

object HighOrderFns {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1 else f(a) * product(f)(a + 1, b)

  def factorial(n: Int): Int = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combinator: (Int, Int) => Int, breaker: Int)(a: Int, b: Int): Int =
    if (a > b) breaker else combinator(f(a), mapReduce(f, combinator, breaker)(a + 1, b))

  def curriedSum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  def curriedProduct(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

}
