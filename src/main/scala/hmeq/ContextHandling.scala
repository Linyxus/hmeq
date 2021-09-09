package hmeq

import Type._


trait ContextHandling:
  def freshTypeVarName(using ctx: Context): String = ctx.freshTypeVarName

  def freshTypeVar(using Context): TypeVar = TypeVar(freshTypeVarName)
