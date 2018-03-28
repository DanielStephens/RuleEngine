package ast

import token.Tokens.Token

import scala.reflect.runtime.universe._

object AbstractSyntaxTreeComponents {

  sealed trait AST

  case object Input extends AST

  case class Variable(name: String) extends AST

  case class Value[T](value: T, tpe : Type) extends AST

  case class Func(name: String, params: List[AST]) extends AST

  case class Holder(token: Token) extends AST

}
