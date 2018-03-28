package environment

import environment.NamespaceTypes.{TypedFunc, TypedValue}

import scala.reflect.runtime.universe._

trait Environment {

  def resolveFunction(name: String, returnType: Type, params: List[Type]) : Either[String, TypedFunc]

  def resolveVariable(name: String, tpe : Type) : Either[String, TypedValue[_]]

}
