package actions

import scala.reflect.runtime.universe._
import actions.Actions.Action
import environment.Namespace
import grouping.Grouper.{asHolder, groupInOrder, parsers, validate}
import grouping.Groups.{Group, Value}
import parsing.TokenLexer.TokenLexerError
import parsing.Tokens.Token

object ActionTreeBuilder {

    def apply(group : Group, namespace : Namespace): Either[TokenLexerError, Action] = ???;//new ContextualTreeBuilder(namespace).build(group).left.map(TokenLexerError)


//    class ContextualTreeBuilder(namespace : Namespace){
//
//        def build(group : Group, expectedType : Type): Either[String, Action] = group match {
//            case Value(v : )
//
//
//        }
//
//    }

}
