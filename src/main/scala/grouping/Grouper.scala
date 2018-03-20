package grouping

import grouping.Groups.{Variable, _}
import parsing.TokenLexer.TokenLexerError
import parsing.Tokens._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers

object Grouper extends Parsers {

    def apply(tokens: Seq[Token]): Either[TokenLexerError, Group] = {
        val groups = groupInOrder(tokens.map(asHolder), parsers)
        validate(groups)
    }

    def value[T]: Parser[Value[T]] = {
        accept("value", { case Holder(v @ VALUE(value : T)) => Value(value, v.tag.tpe) })
    }

    def variable: Parser[Variable] = {
        accept("variable", { case Holder(VARIABLE(name)) => Variable(name) })
    }

    def plus: Parser[Func] = {
        (notHolder ~ Holder(PLUS) ~ notHolder) ^^ { case p1 ~ _ ~ p2 => Func("plus", List(p1, p2))}
    }

    def equals: Parser[Func] = {
        (notHolder ~ Holder(EQUALS) ~ notHolder) ^^ { case p1 ~ _ ~ p2 => Func("equals", List(p1, p2))}
    }

    def where: Parser[Func] = {
        (notHolder ~ notHolder ~ Holder(WHERE) ~ notHolder) ^^ { case Func("has", _) ~ condition ~ where ~ p => Func("where", List(p))}
    }

//    def has: Parser[Func] = {
//        (Value(BigInt) ~ Holder(HAS) ~ notHolder) ^^ { case count ~ has ~ condition => Func("matches", List(count, condition))}
//    }

    def brackets: Parser[Group] = {
        (
            Holder(OPEN) ~> collect(parserGroup, Holder(CLOSE)) <~ Holder(CLOSE)
        ) 
    }

    def collect(using: Parser[Group], until: => Parser[Group]): Parser[List[Group]] =
        Parser { in =>
            val elems = new ListBuffer[Group]
            val p0 = until    // avoid repeatedly re-evaluating by-name parser
            val u0 = using

            @tailrec def applyp(in0: Input): ParseResult[List[Group]] =
                if (in0.atEnd) Failure("expected match was not found", in0)
                else p0(in0) match {
                    case Success(x, rest) => Success(elems.toList, in0)
                    case ns: NoSuccess => {
                        u0(in0) match {
                            case Success(x, rest) => elems += x ; applyp(rest)
                            case ns: NoSuccess => ns
                        }
                    }
                }

            applyp(in)
        }

    def parserGroup: Parser[Group] = {
        parsers.reduce((p1, p2) => p1 | p2)
    }

    def parsers: Seq[Parser[Group]] = {
        Seq(
            brackets,
            value,
            variable,
            plus,
            equals
        )
    }

//    def namedFunction: Parser[Func] = {
//        (Holder(FUNCTION) ~ OPEN ~ argh ~ CLOSE) ^^ { case p1 ~ _ ~ p2 => Func("equals", List(p1, p2))}
//    }

    def groupInOrder(groups : Seq[Group], parsers: Seq[Parser[Group]]) : Seq[Group] = {
        parsers.foldLeft(groups)((seq, parser) => group(seq, parser))
    }

    def group(groups : Seq[Group], parser: Parser[Group]) : Seq[Group] = {
        val seq = phrase(rep(parser | anyGroup))(new GroupReader(groups)).get
        println("reduced\n[ " + groups + " ] to\n[ " + seq + " ]")
        seq
    }

    def notHolder: Parser[Group] = {
        accept("any group other than holder", { case g : Group if !g.isInstanceOf[Holder] => g })
    }

    def anyGroup: Parser[Group] = {
        accept("any group", { case g : Group => g })
    }

    def asHolder(token: Token) : Group = Holder(token)

    def validate(groups : Seq[Group]) : Either[TokenLexerError, Group] = {
        if(groups.size == 1){
            Right(groups.head)
        }else if(groups.size > 1){
            Left(TokenLexerError("The order of execution was ambiguous [ " + groups + " ]"))
        }else{
            Left(TokenLexerError("Nothing to execute"))
        }
    }

    override type Elem = Group

}
