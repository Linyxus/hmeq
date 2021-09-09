import io.StdIn.readLine

import hmeq.HMEqParser
import hmeq.ConstraintGen
import hmeq.Context
import hmeq.Type

@main def repl: Unit = 
  while true do
    val input = readLine("> ")

    if input == ":q" then return

    HMEqParser.parse(input) match
      case Left(err) => println(f"parse failed: $err")
      case Right(expr) => 
        val ctx = new Context
        given Context = ctx

        println(s"parsed $expr")
        val alphaExpr = expr.alpha
        println(s"alpha $alphaExpr")

        val expected = Type.TypeVar(ctx.freshTypeVarName)
        val constraints = ConstraintGen.generate(alphaExpr, expected)
        println(constraints)
