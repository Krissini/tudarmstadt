object Preprocessor {

  /**
   * BooleanExpression represents the abstract syntax of our language,
   * i.e. the representation a parser would generate by reading the program code.
   * This will be consumed by the interpreter (the eval function) to produce a result BooleanValue.
   */
  sealed trait BooleanExpression
  case object True extends BooleanExpression
  case object False extends BooleanExpression
  case class Not(expr: BooleanExpression) extends BooleanExpression
  case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression
  case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression
  case class Imp(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression
  case class BiImp(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // implement this function
  def preproc(expr: BooleanExpression): BooleanExpression = expr match {
    
    case Imp(lhs, rhs) => preproc(lhs) match{
      // A⇒B and ¬A∨B
      case _ => Or.apply((Not.apply(lhs)), rhs)
    }
    
    case BiImp(lhs, rhs) => preproc(lhs) match{
      case _ => And.apply((Or.apply((Not.apply(lhs)), rhs)), (Or.apply((Not.apply(rhs)), lhs)))
    }
    
    case True => True
    case False => False
    case _ => expr
    
    /*
    case Imp(lhs, rhs) => preproc(Or.apply((Not.apply(lhs)), rhs))
    case BiImp(lhs, rhs) => preproc(And.apply((Or.apply((Not.apply(lhs)), rhs)), (Or.apply((Not.apply(rhs)), lhs))))
    case Not(expr) => preproc(Not.apply(expr))
    case And(lhs, rhs) => preproc(And.apply(lhs, rhs))
    case Or(lhs, rhs) => preproc(Or.apply(lhs, rhs))
    case True => True
    case False => False
    case _ => expr
    */
  }
}
