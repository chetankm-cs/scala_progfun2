package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, expr) => name -> Signal(eval(expr(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evalWithCheck(expr: Expr, references: Map[String, Signal[Expr]], parentSet: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) =>
          if (parentSet.contains(name)) Double.NaN
          else
            evalWithCheck(getReferenceExpr(name, references), references, parentSet + name)
        case Plus(a, b) => evalWithCheck(a, references,parentSet) + evalWithCheck(b, references,parentSet)
        case Minus(a, b) => evalWithCheck(a, references,parentSet) - evalWithCheck(b, references,parentSet)
        case Times(a, b) => evalWithCheck(a, references, parentSet) * evalWithCheck(b, references, parentSet)
        case Divide(a, b) => evalWithCheck(a, references, parentSet) / evalWithCheck(b, references, parentSet)
      }
    }
    evalWithCheck(expr,references,Set.empty)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
