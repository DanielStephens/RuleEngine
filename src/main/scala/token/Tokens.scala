package token

import scala.reflect.runtime.universe._
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Tokens {

  sealed trait Token
  sealed trait SpecialFunc extends Token

  case class FUNCTION(val name: String) extends Token
  case class VARIABLE(name: String) extends Token
  case class VALUE[T](value : T)(implicit val tag : TypeTag[T]) extends Token {
    override def toString: String = s"VALUE[${value.getClass.getSimpleName}](${value})"
  }

  case object WHITESPACE extends Token

  case object OPEN extends Token
  case object CLOSE extends Token

  case object COMMA extends Token

  case object EQUALS extends SpecialFunc
  case object PLUS extends SpecialFunc
  case object MINUS extends SpecialFunc
  case object DIVIDE extends SpecialFunc
  case object MULTIPLY extends SpecialFunc

  case object AND extends SpecialFunc
  case object OR extends SpecialFunc
  case object NOT extends SpecialFunc

  case object WHERE extends SpecialFunc
  case object HAS extends SpecialFunc

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

}
