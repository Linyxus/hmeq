package hmeq


enum Constraint:
  case CTrue
  case CFalse
  case Equal(t1: Type, t2: Type)
  case And(c1: Constraint, c2: Constraint)
  case Exists(bindings: List[String], c: Constraint)
  case Def(name: String, scheme: TypeScheme, c: Constraint)
  case Inst(ident: String, tp: Type)

  override def toString: String = this match
    case CTrue => "True"
    case CFalse => "False"
    case Equal(t1, t2) => s"($t1 == $t2)"
    case And(c1, c2) => c1.toString ++ " /\\ " ++ c2.toString
    case Exists(bindings, c) => "(" ++ "∃" ++ bindings.mkString(" ") ++ ", " ++ c.toString ++ ")"
    case Def(name, scheme, c) => s"(def $name : $scheme in $c)"
    case Inst(id, tp) => s"($id <~ $tp)"

object Constraint:
  extension (c: Constraint) {
    def &&(c2: Constraint): Constraint = And(c, c2)
  }

  def Let(name: String, scheme: TypeScheme, c: Constraint): Constraint =
    Def(name, scheme, c) && scheme.constr
