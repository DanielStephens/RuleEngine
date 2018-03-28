package environment
import java.lang.reflect.Method

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object NamespaceTypes {

  sealed trait NamespaceType

  case class TypedFunc(name : String, func : List[Any] => Any, paramTypes : List[Type], returnType : Type) extends NamespaceType {

    def callMethod(args : List[Any]) : Any = func(args)

  }

  case class TypedValue[T](name : String, value : T, tpe : Type) extends NamespaceType

  implicit class Named(name : String) {

    def function[A, R](f : A => R)(implicit a : TypeTag[A], r : TypeTag[R]) = TypedFunc(
      name,
      list => f(list.head.asInstanceOf[A]),
      List(a.tpe),
      r.tpe
    )

    def function[A, B, R](f : (A, B) => R)(implicit a : TypeTag[A], b : TypeTag[B], r : TypeTag[R]) = TypedFunc(
      name,
      list => f(list.head.asInstanceOf[A], list.tail.head.asInstanceOf[B]),
      List(a.tpe, b.tpe),
      r.tpe
    )

    def function[A, B, C, R](f : (A, B, C) => R)(implicit a : TypeTag[A], b : TypeTag[B], c : TypeTag[C], r : TypeTag[R]) = TypedFunc(
      name,
      list => f(list.head.asInstanceOf[A], list.tail.head.asInstanceOf[B], list.tail.tail.head.asInstanceOf[C]),
      List(a.tpe, b.tpe, c.tpe),
      r.tpe
    )

    def variable[A](value : A)(implicit typeTag : TypeTag[A]) = TypedValue(name, value, typeTag.tpe)

  }

}
