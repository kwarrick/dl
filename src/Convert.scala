import scala.util.parsing.combinator._
 
abstract class Expr
case class SExpr(l: List[Expr]) extends Expr
case class SIdent(s: String) extends Expr

object SExprParser extends JavaTokenParsers {
  def sexpr: Parser[Expr] = 
    ident ^^ (SIdent(_)) | 
    "(" ~> rep(sexpr) <~ ")" ^^ (SExpr(_))

  def apply(s: String) = parseAll(sexpr, s)
  
  def main(args: Array[String]) {
    def infix(l: Expr): String = l match {
      case SIdent(s) => s
      case SExpr(List(SIdent("NOT"), e))  =>
        "(NOT " + infix(e) + ")"
      case SExpr(List(SIdent(op), a, b)) => 
        "(" + infix(a) + " " + op + " " + infix(b) + ")"
    }
    println(infix(parseAll(sexpr, args(0))))
  }
}

