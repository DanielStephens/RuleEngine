package base

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait AdvancedParsers extends Parsers {

  def untilNoChange(p: => Parser[Elem]): Parser[List[Elem]] =
    Parser { in =>
      val p0 = p
      val elems = new ListBuffer[Elem]
      var changed = false;

      @tailrec def applyp(in0: Input): ParseResult[List[Elem]] = {
        if (in0.atEnd) Success(elems.toList, in0)
        else p0(in0) match {
          case Success(x, rest) => {
            elems += x
            val reader = compose(new SeqReader(elems.toList), rest)
            elems.clear()
            applyp(reader)
          }
          case e@Error(_, _) => e // still have to propagate error
          case ns: NoSuccess => elems += in0.first; applyp(in0.rest)
        }
      }

      applyp(in)
    }

  def compose(i1: Input, i2: Input): Input = {
    if (i1.atEnd) i2 else new ComposedReader[Elem](i1, i2)
  }

  private class ComposedReader[Elem](firstReader: Reader[Elem], secondReader: Reader[Elem]) extends Reader[Elem] {
    override def first: Elem = firstReader.first

    override def atEnd: Boolean = false

    override def pos: Position = NoPosition

    override def rest: Reader[Elem] = if (firstReader.rest.atEnd) secondReader else new ComposedReader[Elem](firstReader.rest, secondReader)
  }

  class SeqReader[Elem](seq: Seq[Elem]) extends Reader[Elem] {
    override def first: Elem = seq.head

    override def atEnd: Boolean = seq.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Elem] = new SeqReader[Elem](seq.tail)
  }

  private def inputAsList(input: Input): List[Elem] = {
    val elems = new ListBuffer[Elem]
    var i: Input = input

    while (!i.atEnd) {
      elems += i.first
      i = i.rest
    }

    elems.toList
  }

}
