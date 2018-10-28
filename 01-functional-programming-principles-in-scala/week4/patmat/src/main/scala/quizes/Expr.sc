trait Expr{
  def eval(): Int =this match {
    case Number(n) => n
    case Sum(e1 , e2) => e1.eval + e2.eval
    case Prod(e1 , e2) => e1.eval * e2.eval
    case _ => throw new IllegalArgumentException
  }

  def _wrap_paren(e: Expr): Boolean = e match {
    case Sum(_, _) => true
    case _ => false
  }

  def _wrap_paren_if_needed(e: Expr): String = e match {
    case Sum(a, b) => "(" + e.show + ")"
    case _ => e.show
  }

  def show(): String = this match {
    case Number(n) => n.toString
    case Sum(e1 , e2) => e1.show + "+" +e2.show
    case Prod(e1 , e2) => _wrap_paren_if_needed(e1) + "*" +_wrap_paren_if_needed(e2)
    case Var(s) => s
    case _ => throw new IllegalArgumentException
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(s: String) extends Expr

println("hello world")



println(Sum(Prod(Number(2), Var("x")), Var("y")).show)
println(Prod(Sum(Number(2), Var("x")), Var("y")).show)