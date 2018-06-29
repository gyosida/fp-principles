package lessons.week6

object PolynomialMaps {

  class Poly(terms0: Map[Int, Double]) {

    val terms = terms0 withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString: String =
      (for ((exp, coeff) <- terms0.toList.sorted.reverse) yield coeff + "x^" + exp) mkString "+"

  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

  p1 + p2

}
