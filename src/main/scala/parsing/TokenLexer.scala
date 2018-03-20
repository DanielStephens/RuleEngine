package parsing

import parsing.Tokens._

import scala.util.parsing.combinator.RegexParsers

object TokenLexer extends RegexParsers {

    def apply(code: String): Either[TokenLexerError, Seq[Token]] = {
        parse(tokens, code) match {
            case NoSuccess(msg, _) => Left(TokenLexerError(msg))
            case Success(result, _) => Right(result)
        }
    }

    def tokens: Parser[Seq[Token]] = {
        val parser: Parser[Token] = equals | and | or | plus | minus | divide | multiply | open | close | comma | where | has | function | variable | string | decimal | wholeNumber
        phrase(rep1(parser)) ^^ (matchBrackets) ^?({ case Right(l) => l }, t => t.left.get.msg)
    }

    def open = "(" ^^ (_ => OPEN)

    def close = ")" ^^ (_ => CLOSE)

    def equals = ("=" | "equals") ^^ (_ => EQUALS)

    def and = "and" ^^ (_ => AND)

    def or = "or" ^^ (_ => OR)

    def plus = "+" ^^ (_ => PLUS)

    def minus = "-" ^^ (_ => MINUS)

    def divide = "/" ^^ (_ => DIVIDE)

    def multiply = "*" ^^ (_ => MULTIPLY)

    def comma = "," ^^ (_ => COMMA)

    def where = "where" ^^ (_ => WHERE)

    def has = ("has" | "have") ^^ (_ => HAS)

    def function: Parser[FUNCTION] = {
        (variable <~ open) ^^ { str => FUNCTION(str.str) }
    }

    def variable: Parser[VARIABLE] = {
        "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VARIABLE(str) }
    }

    def string: Parser[VALUE[String]] = {
        """"[^"]*"""".r ^^ { str =>
            val content = str.substring(1, str.length - 1)
            VALUE(content)
        }
    }

    def wholeNumber: Parser[VALUE[BigInt]] = {
        "[0-9]+".r ^^ { str => VALUE(BigInt(str))
        }
    }

    def decimal: Parser[VALUE[BigDecimal]] = {
        """[0-9]*\.[0-9]+""".r ^^ { str => VALUE(BigDecimal(str))
        }
    }

    def matchBrackets(tokens : Seq[Token]): Either[TokenLexerError, Seq[Token]] = {
        if(tokens.collect {
            case OPEN => 1
            case CLOSE => -1
        }.sum == 0){
            Right(tokens)
        }else{
            Left(TokenLexerError("Unmatched brackets exist in the statement"))
        }
    }

    override def skipWhitespace = true

    trait CompilationError

    case class TokenLexerError(msg: String) extends CompilationError


}
