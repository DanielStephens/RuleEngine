package ast

import base.AdvancedParsers
import exceptions.Exceptions.ASTParsingError
import ast.AbstractSyntaxTreeComponents._
import token.Tokens._

object AbstractSyntaxTreeBuilder extends AdvancedParsers {

  def apply[T](tokens: Seq[Token]): Either[ASTParsingError, AST] = {
    val holders = asHolders(tokens)
    untilNoChange(compoundParser)(new AbstractSyntaxTreeBuilder.SeqReader[AST](holders)) match {
      case NoSuccess(msg, _) => Left(ASTParsingError(msg))
      case Success(result, _) => validateGrouping(result)
    }
  }

  override type Elem = AST

  private def value[T]: Parser[AST] = {
    accept("value", { case Holder(v @ VALUE(value: T)) => Value(value, v.tag.tpe) })
  }

  private def variable: Parser[AST] = {
    accept("variable", { case Holder(VARIABLE(name: String)) => Variable(name) })
  }

  private def functionName: Parser[String] = {
    accept("function name", { case Holder(FUNCTION(name)) => name })
  }

  private def equals: Parser[AST] = binaryMatcher("equal", EQUALS)

  private def plus: Parser[AST] = binaryMatcher("plus", PLUS)

  private def minus: Parser[AST] = binaryMatcher("minus", MINUS)

  private def divide: Parser[AST] = binaryMatcher("divide", DIVIDE)

  private def multiply: Parser[AST] = binaryMatcher("multiply", MULTIPLY)

  private def and: Parser[AST] = binaryMatcher("and", AND)

  private def or: Parser[AST] = binaryMatcher("or", OR)

  private def not: Parser[AST] = {
    (Holder(NOT) ~ notHolder) ^^ { case _ ~ p => Func("not", List(p)) }
  }

  private def namedFunction: Parser[AST] = {
    (functionName ~ Holder(OPEN) ~ repsep(compoundParser, Holder(COMMA)) ~ Holder(CLOSE)) ^^ {
      case name ~ _ ~ params ~ _ => Func(name, params)
    }
  }

  private def brackets: Parser[AST] = {
    Holder(OPEN) ~> compoundParser <~ Holder(CLOSE)
  }

  private def compoundParser: Parser[AST] = {
    parsers.reduce((p1, p2) => p1 | p2)
  }

  private def parsers: List[Parser[AST]] = {
    List(
      value,
      variable,
      namedFunction,
      equals,
      plus,
      minus,
      divide,
      multiply,
      and,
      or,
      not,
      brackets
    )
  }

  private def binaryMatcher[T <: Token](name: String, t: T): Parser[AST] = {
    (notHolder ~ Holder(t) ~ notHolder) ^^ { case p1 ~ _ ~ p2 => Func(name, List(p1, p2)) }
  }

  def asHolders(tokens: Seq[Token]): Seq[AST] = {
    tokens.map(Holder)
  }

  private def notHolder: Parser[AST] = {
    accept("not a holder", {
      case t: AST if !t.isInstanceOf[Holder] => t
    })
  }

  def validateGrouping(tokens: Seq[AST]): Either[ASTParsingError, AST] = {
    val unparsedTags = tokens.collect { case Holder(_) => 1 }.size
    val ungroupedTags = tokens.size - 1

    if (ungroupedTags + unparsedTags > 0) {
      Left(ASTParsingError("Holder tags still remain or executions had ambiguous order : " + tokens))
    } else {
      Right(tokens.head)
    }
  }
}
