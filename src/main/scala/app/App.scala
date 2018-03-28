package app

import environment.SimpleEnvironment
import environment.NamespaceTypes._
import ast.AbstractSyntaxTreeBuilder
import token.TokenLexer
import action.ActionBuilder

object App {

  def main(args : Array[String]): Unit = {
    val tokens = TokenLexer("var + plus(1,2) = 4")
    println(tokens)
    val ast = tokens.flatMap(t => AbstractSyntaxTreeBuilder(t))
    println(ast)

    val equals = "equal".function((a : Any, b : Any) => a == b)
    val plus = "plus".function((a: BigInt, b: BigInt) => a + b)
    val variable = "var".variable(BigInt(2))

    val environment = new SimpleEnvironment(List(
      equals,
      plus,
      variable
    ))

    ast.flatMap(a => ActionBuilder[Any, Boolean](a, environment)) match {
      case Right(answer) =>
        println(answer)
        println(answer(1))
      case Left(error) => println(error)
    }
  }

}
