package action

import scala.reflect.runtime.universe._

object Actions {

  sealed trait Action[T] {
    def tpe : Type

    def asAction[R] : T => R
  }

  abstract class TypedAction[I, O](action : I => O) extends Action[I] {
    override def asAction[R] : I => R = i => action(i).asInstanceOf[R]
  }

  case class ValueAction[I, T](value : T, tpe : Type) extends TypedAction[I, T](_ => value)

  case class FuncAction[I, O](action : I => O, tpe : Type) extends TypedAction[I, O](action)

}
