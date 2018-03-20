import grouping.Grouper
import parsing.TokenLexer

object App {

    def main(args : Array[String]): Unit ={
        val tokens = TokenLexer("(1 + 1) = 2")
        println(tokens)
        println(tokens.flatMap(t => Grouper(t)))

//        println(tokens.flatMap(t => FunctionParser(t)))
    }

}
