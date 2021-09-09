package hmeq

enum Type:
  case Int
  case Bool
  case Lambda(arg: Type, value: Type)
  case TypeVar(name: String)

  override def toString: String = this match
    case Int => "int"
    case Bool => "bool"
    case Lambda(arg, value) => s"($arg => $value)"
    case TypeVar(name) => name
