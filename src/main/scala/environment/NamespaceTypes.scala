package environment

import scala.reflect.runtime.universe._

class NamespaceTypes {

    sealed trait NamespaceType {
        def name: String
    }

    case class NamespaceFunction(name : String, func: List[Any] => Any, returnType : Type, paramTypes : List[Type])

    case class NamespaceVariable(name : String, tpe : Type)

}
