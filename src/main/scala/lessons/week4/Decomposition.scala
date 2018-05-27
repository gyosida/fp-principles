package lessons.week4

// Decomposition in a OO approach: not optimal
//disadvantages: difficult to add new operations, since has
//to deal with all classes of the hierarchy

// Create a set of expressions
trait Expr {
  val eval: Int
}

class Number(val n: Int) extends Expr {
  override val eval: Int = n
}

class Sum(val e1: Expr, e2: Expr) extends Expr {
  override val eval: Int = e1.eval + e2.eval
}

// Improve solution: Pattern Matching
