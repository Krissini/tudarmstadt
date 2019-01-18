object Interpreter {

  /** BooleanExpression represents the abstract syntax of our language,
    * i.e. the representation a parser would generate by reading the program code.
    * This will be consumed by the interpreter (the eval function) to produce a result BooleanValue. */
  sealed trait BooleanExpression
  case object True extends BooleanExpression
  case object False extends BooleanExpression
  case class Not(expr: BooleanExpression) extends BooleanExpression
  case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression
  case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

  /** BooleanValue represents the result of our interpreter, it is conceptually identical to Scalas build in Boolean type,
    * but we we wanted to be very explicit here, that an interpreter will generally always take expressions which you defined,
    * and return values that you also defined. Neither have to be built in types. */
  sealed trait BooleanValue
  case object TrueValue extends BooleanValue
  case object FalseValue extends BooleanValue

  // only modify this function
  def interp(expr: BooleanExpression): BooleanValue = expr match {
    case True => TrueValue
    case And(lhs, rhs) => interp(lhs) match {
      case FalseValue => FalseValue
      case TrueValue => interp(rhs)
    }

    // --------------------------------------------
    // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
    // --------------------------------------------

    case Not(expr) => interp(expr) match {
      case FalseValue => TrueValue
      case TrueValue => FalseValue
    }
    
    case Or(lhs, rhs) => interp(lhs) match {
      case TrueValue => TrueValue
      case FalseValue => interp(rhs)
    }
    
    case _ => FalseValue
  }

}