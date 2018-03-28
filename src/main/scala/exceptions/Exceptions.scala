package exceptions

object Exceptions {

  trait CompilationError

  case class TokenLexerError(msg: String) extends CompilationError

  case class ASTParsingError(msg: String) extends CompilationError

  abstract class ActionTreeBuildingError extends CompilationError {
    def msg: String
  }

  case class InvalidGroupError(msg : String) extends ActionTreeBuildingError

  case class UnknownPropertyError(msg : String) extends ActionTreeBuildingError

  case class ClassCastException(msg : String) extends ActionTreeBuildingError

  case class UnknownFunctionError(msg : String) extends ActionTreeBuildingError

}
