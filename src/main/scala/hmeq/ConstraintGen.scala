package hmeq

object ConstraintGen extends ContextHandling:
  import Expr._
  import Constraint.{Let => CLet, _}
  import TypeScheme._

  def generate(e: Expr, tp: Type)(using Context): Constraint = e match
    case Var(name) => Inst(name, tp)
    case Abs(argName, body) =>
      val x1 = freshTypeVar
      val x2 = freshTypeVar
      Exists(
        List(x1.name, x2.name), 
        Constraint.Let(argName, x1.toTypeScheme, generate(body, x2)) && Equal(Type.Lambda(x1, x2), tp)
      )
    case App(func, arg) =>
      val x2 = freshTypeVar
      Exists(
        List(x2.name),
        generate(func, Type.Lambda(x2, tp)) && generate(arg, x2)
      )
    case Let(z, t1, t2) =>
      val x = freshTypeVar
      Constraint.Let(
        z, TypeScheme(x.name :: Nil, generate(t1, x), x),
        generate(t2, tp)
      )
    case True | False => Equal(Type.Bool, tp)
    case Zero => Equal(Type.Int, tp)
    case Succ(t) => generate(t, tp) && Equal(Type.Int, tp)
    case Pred(t) => generate(t, tp) && Equal(Type.Int, tp)
