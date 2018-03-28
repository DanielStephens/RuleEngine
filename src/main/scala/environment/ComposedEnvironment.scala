package environment
import scala.reflect.runtime.universe._

class ComposedEnvironment(higherPriority : Environment, lowerPriority : Environment) extends Environment {

  override def resolveFunction(name: String, returnType: Type, params: List[Type]): Either[String, NamespaceTypes.TypedFunc] = {
    val first = higherPriority.resolveFunction(name, returnType, params)
    if(first.isRight) first else lowerPriority.resolveFunction(name, returnType, params)
  }

  override def resolveVariable(name: String, tpe: Type): Either[String, NamespaceTypes.TypedValue[_]] = {
    val first = higherPriority.resolveVariable(name, tpe)
    if(first.isRight) first else lowerPriority.resolveVariable(name, tpe)
  }

}
