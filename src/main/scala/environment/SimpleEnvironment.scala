package environment

import environment.NamespaceTypes.{NamespaceType, TypedFunc, TypedValue}

import scala.reflect.runtime.universe._

class SimpleEnvironment(namespace : List[NamespaceType]) extends Environment {

  override def resolveFunction(name: String, returnType: Type, params: List[Type]) : Either[String, TypedFunc] = {
    val possibilities : List[TypedFunc] = namespace
      .collect {
        case f @ TypedFunc(_, _, _, _) if functionMatcher(f, name, returnType, params) => f
      }

    if (possibilities.size == 1)  Right(possibilities.head)
    else if (possibilities.isEmpty) Left(s"No functions found for the signature <${signatureFrom(name, returnType, params)}>")
    else Left(s"Ambiguous function call found, more than one function matched the signature <${signatureFrom(name, returnType, params)}>")
  }

  override def resolveVariable(name: String, tpe : Type) : Either[String, TypedValue[_]] = {

    val possibilities : List[TypedValue[_]] = namespace
      .collect {
        case v @ TypedValue(n, _, _) if (name == n) => v
      }
    if (possibilities.size == 1)
      if (possibilities.head.tpe <:< tpe) Right(possibilities.head)
      else Left(s"The variable found with name ${name} was not of the type ${tpe}")
    else if (possibilities.isEmpty) Left(s"No variables by the name '${name}' were found")
    else Left(s"Multiple variables by the name '${name}' were found")

  }

  private def signatureFrom(name: String, returnType: Type, params: List[Type]) : String = {
    val args = params.map(_.toString).reduce((s1, s2) => s"${s1} , ${s2}")
    s"${returnType} ${name}(${args})"
  }

  private def functionMatcher[T](func : TypedFunc, name: String, returnType: Type, params: List[Type]) : Boolean = {
    val simpleEquality = func.name == name && func.returnType <:< returnType && params.size == func.paramTypes.size
    simpleEquality && (params.zip(func.paramTypes).forall(ts => ts._1 <:< ts._2))
  }

}
