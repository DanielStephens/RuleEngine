package environment

import environment.NamespaceTypes.{NamespaceType}

import scala.collection.mutable.ListBuffer

object Namespace {

  def newNamespace : ListBuffer[NamespaceType] = new ListBuffer[NamespaceType]
//  +=
//    new Func("equal", (a : AnyRef, b : AnyRef) => (a == b)) +=
//    new Func("plus", (a : BigDecimal, b : BigDecimal) => (a + b))

}
