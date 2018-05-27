package lessons.week1

// SqrtRoot.sqrt(4) == 2
class SqrtRoot {

  def sqrt(x: Double): Double = {
    def abs(): Double = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = Math.abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }

}
