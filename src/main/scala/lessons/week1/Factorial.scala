package lessons.week1

// Factorial.factorial(4) == 24
class Factorial {

  def factorial(n: Int): Int = {
    def aux(n: Int, acc: Int): Int =
      if (n == 0) acc else aux(n - 1, n * acc)
    aux(n, 1)
  }

}
