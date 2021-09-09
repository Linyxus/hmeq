package hmeq

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._


object HMEqParser extends StandardTokenParsers:
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in")

  import Expr._

  def expr: Parser[Expr] =
    term ~ rep(term) ^^ { case t ~ ts => (t :: ts).reduceLeft[Expr](App.apply) }
    | failure("illegal start of term")

  def term: Parser[Expr] = 
    "true" ^^^ True
    | "false" ^^^ False
    | numericLit ^^ { chars => lit2Num(chars.toInt) }
    | "succ" ~ expr ^^ { case _ ~ t => Succ(t) }
    | "pred" ~ expr ^^ { case _ ~ t => Pred(t) }
    | ident ^^ { case id => Var(id) }
    | "\\" ~ ident ~ "." ~ expr ^^ {
        case _ ~ x ~ _ ~ body => Abs(x, body)
      }
    | "(" ~> expr <~ ")"  ^^ { case t => t }
    | "let" ~ ident ~ "=" ~ expr ~ "in" ~ expr ^^ {
        case _ ~ x ~ _ ~ value ~ _ ~ body => Let(x, value, body)
      }
    | failure("illegal start of simple term")

  private def lit2Num(n: Int): Expr =
    if (n == 0) Zero else Succ(lit2Num(n - 1))

  def parse(input: String): Either[String, Expr] =
    val tokens = new lexical.Scanner(input)
    phrase(expr)(tokens) match
      case Success(expr, _) => Right(expr)
      case e => Left(e.toString)
