package action

import exceptions.Exceptions.{ActionTreeBuildingError, _}
import environment.SimpleEnvironment
import environment.NamespaceTypes.TypedFunc
import ast.AbstractSyntaxTreeComponents._
import action.Actions._

import scala.reflect.runtime.universe._
import scala.util.parsing.combinator.Parsers

object ActionBuilder extends Parsers {

  def apply[I, O](group: AST, environment: SimpleEnvironment)(implicit input : TypeTag[I], output : TypeTag[O]): Either[ActionTreeBuildingError, I => O] = {
    new Evaluator(environment).evaluate[I](group, input.tpe, output.tpe).map(_.asAction[O])
  }

  class Evaluator(environment: SimpleEnvironment) {

    def evaluate[I](group: AST, expectedType : Type, inputType : Type) : Either[ActionTreeBuildingError, Action[I]] = group match {
      case Input if  inputType <:< expectedType => Right(FuncAction(i => i, inputType))
      case Input => Left(InvalidGroupError(s"Could not resolve a variable of type ${expectedType}"))
      case Variable(name) => environment.resolveVariable(name, expectedType)
        .map(t => ValueAction[I, Any](t.value, t.tpe))
          .left.map(UnknownPropertyError)
      case Value(value, tpe) if tpe <:< expectedType => Right(ValueAction(value, tpe))
      case Value(_, tpe) => Left(ClassCastException(s"Could not cast value of type ${tpe} to the expected type ${expectedType}"))
      case Func(name, params) =>
        val paramActions : Either[ActionTreeBuildingError, List[Action[I]]] = sequence(params.map(p => evaluate[I](p, typeOf[Any], inputType)))
        val paramTypes = paramActions.map(_.map(a => a.tpe))
        val function = paramTypes.flatMap(p => environment.resolveFunction(name, expectedType, p).left.map(UnknownFunctionError))
        function.flatMap(f => paramActions.map(p => closeFunction[I](f, p)))
      case other : Any => Left(InvalidGroupError(s"A previous stage of compilation was unsuccessful but was not caught, or there is a new unhandled token type of ${other}"))
    }

    private def closeFunction[I](func : TypedFunc, params : List[Action[I]]) : Action[I] = {
      FuncAction(i => func.callMethod(params.map(_.asAction[Any](i))), func.returnType)
    }

    private def sequence[L, R](l: List[Either[L, R]]): Either[L, List[R]] = l match {
      case Nil => Right(Nil)
      case head :: rest => head match {
        case Left(l) => Left(l)
        case Right(h) => sequence(rest) match {
          case Left(l) => Left(l)
          case Right(list) => Right(h :: list)
        }
      }
    }

  }


}
