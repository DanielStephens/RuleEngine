package `type`

object Types {

//  sealed trait TokenType
//
//  abstract class DecimalNumber(token : String) extends TokenType {
//
//    lazy val asFloat : Float = token.toFloat
//
//    lazy val asDouble : Double = token.toDouble
//
//    lazy val asBigDecimal : BigDecimal = BigDecimal(token)
//
//  }
//
//  case class WholeNumberLiteral(val token : String) extends DecimalNumber(token) {
//
//    lazy val asInt : Integer = token.toInt
//
//    lazy val asLong : Long = token.toLong
//
//    lazy val asBigInt : BigInt = BigInt(token)
//
//    lazy val asBoolean : Boolean = asInt == 0
//
//  }
//
//  case class DecimalNumberLiteral(val token: String) extends DecimalNumber(token)
//
//  case class StringLiteral(token : String) extends TokenType
//
//  case class BooleanLiteral(token : String) {
//    lazy val asBoolean : Boolean = token.toBoolean
//  }

  case class Group(records : List[Record])

  case class Record(data : Map[String, List[Any]])

}
