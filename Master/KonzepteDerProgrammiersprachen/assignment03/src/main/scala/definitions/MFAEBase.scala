package definitions

object MFAEBase {

  /** MFAE: multiple-argument functions and arithmetic expressions */
  sealed trait MFAE
  case class MNum(n: Int) extends MFAE
  case class MAdd(lhs: MFAE, rhs: MFAE) extends MFAE
  case class MSub(lhs: MFAE, rhs: MFAE) extends MFAE
  case class MId(name: Symbol) extends MFAE
  case class MFun(params: List[Symbol], body: MFAE) extends MFAE
  case class MApp(funExpr: MFAE, args: List[MFAE]) extends MFAE

  def substMultiarg(expr: MFAE, substId: Symbol, value: MFAE): MFAE = expr match {
    case MNum(n) =>
      expr
    case MAdd(lhs, rhs) =>
      MAdd(substMultiarg(lhs, substId, value), substMultiarg(rhs, substId, value))
    case MSub(lhs, rhs) =>
      MSub(substMultiarg(lhs, substId, value), substMultiarg(rhs, substId, value))
    case MId(name) =>
      if (substId == name) value
      else expr
    case MFun(args, body) =>
      if (args.contains(substId)) expr
      else MFun(args, substMultiarg(body, substId, value))
    case MApp(funExpr, argsExpr) =>
      MApp(substMultiarg(funExpr, substId, value), argsExpr map (substMultiarg(_, substId, value)))
  }

  def interpMultiarg(expr: MFAE): MFAE = expr match {
    case MNum(n) => expr
    case MFun(arg, body) => expr
    case MId(name) => sys.error("found unbound id " + name)
    case MAdd(lhs, rhs) => MNum(fromMNum(interpMultiarg(lhs)) + fromMNum(interpMultiarg(rhs)))
    case MSub(lhs, rhs) => MNum(fromMNum(interpMultiarg(lhs)) - fromMNum(interpMultiarg(rhs)))
    case MApp(funExpr, argsExpr) => interpMultiarg(funExpr) match {
      case MFun(params, body) =>
        val substBody = (params zip argsExpr).foldLeft(body) {
          case (oldBody, (argName, argExpr)) => substMultiarg(oldBody, argName, interpMultiarg(argExpr))
        }
        interpMultiarg(substBody)
      case _ => sys.error("Can only handle function expressions")
    }
  }

  def fromMNum(expr: MFAE): Int = expr match {
    case num: MNum => num.n
    case _ => sys.error("Can only handle numbers")
  }

}