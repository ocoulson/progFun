package Week4

/**
  * Created by Oliver Coulson on 04/06/2017.
  */


trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(l, r) => l.eval + r.eval
    case Product(l, r) => l.eval * r.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(l,r) => s"${l.show} + ${r.show}"
    case Product(l,r) => {
      val left = l match {
        case Sum(_,_) => s"(${l.show})"
        case _ => l.show
      }
      val right = r match {
        case Sum(_,_) => s"(${r.show})"
        case _ => r.show
      }
      s"$left * $right"
    }
    case Var(x) => x
  }

}
case class Number(n: Int) extends Expr
case class Sum(left: Expr, right: Expr) extends Expr
case class Product(left: Expr, right: Expr) extends Expr
case class Var(name: String) extends Expr

object Test extends App {
  println(Sum(Product(Number(2), Var("x")), Sum(Number(1), Product(Var("y"), Number(44)))).show)

  println(Product(Sum(Number(2), Var("x")), Var("y")).show)
  println(Sum(Product(Number(2), Var("x")), Var("y")).show)

}


