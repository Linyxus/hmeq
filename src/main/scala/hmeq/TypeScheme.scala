package hmeq

case class TypeScheme(args: List[String], constr: Constraint, tp: Type):
  override def toString: String = 
    "∀ " ++ args.mkString(" ") ++ " [" ++ constr.toString ++ "], " ++ tp.toString

object TypeScheme:
  def fromType(tp: Type): TypeScheme = TypeScheme(Nil, Constraint.CTrue, tp)

  extension (tp: Type) {
    def toTypeScheme: TypeScheme = fromType(tp)
  }
