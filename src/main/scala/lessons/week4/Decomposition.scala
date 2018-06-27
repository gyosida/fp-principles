package lessons.week4

// Decomposition in a OO approach: not optimal
//disadvantages: difficult to add new operations, since has
//to deal with all classes of the hierarchy

// Create a set of expressions
trait OOExpr {
  val eval: Int
}

class OONumber(val n: Int) extends OOExpr {
  override val eval: Int = n
}

class OOSum(val e1: OOExpr, e2: OOExpr) extends OOExpr {
  override val eval: Int = e1.eval + e2.eval
}

// Improve solution: Pattern Matching
// extensible for new methods
trait Expr {
  def eval(): Int = this match {
    case Number(n) => n
    case Sum(x, y) => x.eval + y.eval
  }
}
case class Number(n: Int) extends Expr
case class Sum(x: Expr, y: Expr) extends Expr

object Expr {
  def show(expr: Expr): String = expr match {
    case Number(n) => n.toString
    case Sum(x, y) => show(x) + " + " + show(y)
  }
}
