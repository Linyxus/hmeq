package hmeq

enum Expr:
  // syntax
  case Var(name: String)
  case Abs(argName: String, body: Expr)
  case App(func: Expr, arg: Expr)
  case Let(name: String, value: Expr, body: Expr)
  // constants
  // boolean
  case True
  case False
  // natural
  case Zero
  case Succ(t: Expr)
  case Pred(t: Expr)

object Expr:
  def alphaConversion(e: Expr): Expr =
    var i = 0
    def loop(e: Expr, subst: Map[String, String]): Expr = e match
      case Var(name) if subst contains name => Var(subst(name))
      case Abs(argName, body) =>
        val newName = argName + "$" + i.toString
        i += 1
        Abs(newName, loop(body, subst.updated(argName, newName)))
      case Let(name, value, body) =>
        val newName = name + "$" + i.toString
        i += 1
        Let(newName, loop(value, subst), loop(body, subst.updated(name, newName)))
      case Succ(t) => Succ(loop(t, subst))
      case Pred(t) => Pred(loop(t, subst))
      case App(t1, t2) =>
        App(loop(t1, subst), loop(t2, subst))
      case e => 
        e

    loop(e, Map.empty)

  extension (e: Expr) {
    def alpha: Expr = alphaConversion(e)
  }
