package lessons.week2

class Rational(x: Int, y: Int) {
  require(y > 0, "denominator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int =
    if (y == 0) x else gcd(y, x % y)

  val numer: Int = x
  val denom: Int = y

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean = numer * that.denom > that.numer * denom

  def max(that: Rational): Rational = if (this < that) that else this

  override def toString: String = {
    val gcdValue = gcd(numer, denom)
    numer / gcdValue + "/" + denom / gcdValue
  }

}