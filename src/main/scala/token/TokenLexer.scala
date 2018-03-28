package token

import exceptions.Exceptions.TokenLexerError
import token.Tokens._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.RegexParsers

object TokenLexer extends RegexParsers {

  def apply(code: String): Either[TokenLexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(TokenLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      and | or | not | where | has | open | close | comma | equals | plus | minus | divide | multiply | function | variable | literal | decimal | wholeNumber)
    ) ^^ {rawTokens => matchBrackets(rawTokens) } ^? { case Some(list) => list }
  }

  def open = "(" ^^ (_ => OPEN)
  def close = ")" ^^ (_ => CLOSE)

  def comma = "," ^^ (_ => COMMA)

  def equals = "=" ^^ (_ => EQUALS)
  def plus = "+" ^^ (_ => PLUS)
  def minus = "-" ^^ (_ => MINUS)
  def divide = "/" ^^ (_ => DIVIDE)
  def multiply = "*" ^^ (_ => MULTIPLY)

  def and = "and" ^^ (_ => AND)
  def or = "or" ^^ (_ => OR)
  def not = "not" ^^ (_ => NOT)

  def where = "where" ^^ (_ => WHERE)
  def has = ("has" | "have") ^^ (_ => HAS)

  def variable: Parser[VARIABLE] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VARIABLE(str) }
  }

  def function: Parser[FUNCTION] = {
    (variable <~ matchWithoutUsingUp(open)) ^^ { str => FUNCTION(str.name) }
  }

  def literal: Parser[VALUE[String]] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      VALUE(content)
    }
  }

  def wholeNumber: Parser[VALUE[BigInt]] = {
    "[0-9]+".r ^^ { str => VALUE(BigInt(str)) }
  }

  def decimal: Parser[VALUE[BigDecimal]] = {
    """[0-9]*\.[0-9]+""".r ^^ { str => VALUE(BigDecimal(str)) }
  }

  def matchBrackets(tokens : List[Token]): Option[List[Token]] = {
    val matchedBrackets : Boolean = tokens.collect {
      case OPEN => 1
      case CLOSE => -1
    }.sum == 0

    if(matchedBrackets){
      Option(tokens)
    }else{
      None
    }
  }

  def matchWithoutUsingUp[T](p: => Parser[T]): Parser[T] = { in =>
      val elems = new ListBuffer[T]
      val p0 = p // avoid repeatedly re-evaluating by-name parser

      def applyp(in0: Input): ParseResult[T] =
        p0(in0) match {
          case Success(x, _) => Success(x, in0)
          case ns: NoSuccess    => ns
        }

      applyp(in)
    }

}
