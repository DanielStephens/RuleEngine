package grouping

import scala.reflect.runtime.universe._
import parsing.Tokens.Token

import scala.util.parsing.input.{NoPosition, Position, Reader}

object Groups {

    sealed trait Group

    case class Variable(name : String) extends Group

    case class Value[T](value : T, tpe : Type) extends Group

    case class Func(name : String, params : List[Group]) extends Group

    case class Holder(t : Token) extends Group

    class GroupReader(tokens: Seq[Group]) extends Reader[Group] {
        override def first: Group = tokens.head
        override def atEnd: Boolean = tokens.isEmpty
        override def pos: Position = NoPosition
        override def rest: Reader[Group] = new GroupReader(tokens.tail)
    }

}
