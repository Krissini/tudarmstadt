package definitions

object FLAEBase {

  /** FLAE: functions, let and arithmetic expressions */
  sealed trait FLAE
  case class LNum(n: Int) extends FLAE
  case class LAdd(lhs: FLAE, rhs: FLAE) extends FLAE
  case class LSub(lhs: FLAE, rhs: FLAE) extends FLAE
  case class LLet(name: Symbol, namedExpr: FLAE, body: FLAE) extends FLAE
  case class LId(name: Symbol) extends FLAE
  case class LFun(param: Symbol, body: FLAE) extends FLAE
  case class LApp(funExpr: FLAE, arg: FLAE) extends FLAE

  def substLet(expr: FLAE, substId: Symbol, value: FLAE): FLAE = expr match {
    case LNum(n) =>
      expr
    case LAdd(lhs, rhs) =>
      LAdd(substLet(lhs, substId, value), substLet(rhs, substId, value))
    case LSub(lhs, rhs) =>
      LSub(substLet(lhs, substId, value), substLet(rhs, substId, value))
    case LId(name) =>
      if (substId == name) value
      else expr
    case LFun(arg, body) =>
      if (arg == substId) expr
      else LFun(arg, substLet(body, substId, value))
    case LApp(funExpr, argExpr) =>
      LApp(substLet(funExpr, substId, value), substLet(argExpr, substId, value))
    case LLet(boundId, namedExpr, boundBody) =>
      val substNamedExpr = substLet(namedExpr, substId, value)
      val substBoundBody =
        if (boundId == substId) boundBody
        else substLet(boundBody, substId, value)
      LLet(boundId, substNamedExpr, substBoundBody)
  }

  def interpLet(expr: FLAE): FLAE = expr match {
    case LNum(n) => expr
    case LFun(arg, body) => expr
    case LId(name) => sys.error("found unbound id " + name)
    case LAdd(lhs, rhs) => LNum(fromLNum(interpLet(lhs)) + fromLNum(interpLet(rhs)))
    case LSub(lhs, rhs) => LNum(fromLNum(interpLet(lhs)) - fromLNum(interpLet(rhs)))
    case LApp(funExpr, argExpr) => interpLet(funExpr) match {
      case LFun(param, body) =>
        interpLet(substLet(body, param, interpLet(argExpr)))
      case _ =>
        sys.error("Can only handle function expressions")
    }
    case LLet(boundId, namedExpr, boundBody) =>
      interpLet(substLet(boundBody, boundId, interpLet(namedExpr)))
  }

  def fromLNum(expr: FLAE): Int = expr match {
    case num: LNum => num.n
    case _ => sys.error("Can only handle numbers")
  }

}