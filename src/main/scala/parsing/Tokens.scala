package parsing

import scala.reflect.runtime.universe._
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Tokens {

    sealed trait Token
    sealed trait ReservedBinaryToken extends Token

    case class VARIABLE(str: String) extends Token
    case class FUNCTION(str: String) extends Token
    case class VALUE[T](value: T)(implicit val tag : TypeTag[T]) extends Token {
        override def toString: String = s"VALUE[${value.getClass.getSimpleName}](${value})"
    }

    case object OPEN extends Token
    case object CLOSE extends Token

    case object WHITESPACE extends Token

    case object EQUALS extends ReservedBinaryToken
    case object AND extends ReservedBinaryToken
    case object OR extends ReservedBinaryToken
    case object PLUS extends ReservedBinaryToken
    case object MINUS extends ReservedBinaryToken
    case object DIVIDE extends ReservedBinaryToken
    case object MULTIPLY extends ReservedBinaryToken

    case object COMMA extends Token

    case object WHERE extends Token

    case object HAS extends Token

    sealed trait Count extends Token
    case object ALL extends Count

    class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
        override def first: Token = tokens.head
        override def atEnd: Boolean = tokens.isEmpty
        override def pos: Position = NoPosition
        override def rest: Reader[Token] = new TokenReader(tokens.tail)
    }


}
