package definitions

object FAEBase {

  /** FAE: functions and arithmetic expressions */
  sealed trait FAE
  case class Num(n: Int) extends FAE
  case class Add(lhs: FAE, rhs: FAE) extends FAE
  case class Sub(lhs: FAE, rhs: FAE) extends FAE
  case class Id(name: Symbol) extends FAE
  case class Fun(param: Symbol, body: FAE) extends FAE
  case class App(funExpr: FAE, arg: FAE) extends FAE

  def subst(expr: FAE, substId: Symbol, value: FAE): FAE = expr match {
    case Num(n) =>
      expr
    case Add(lhs, rhs) =>
      Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs) =>
      Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Id(name) =>
      if (substId == name) value
      else expr
    case Fun(arg, body) =>
      if (arg == substId) expr
      else Fun(arg, subst(body, substId, value))
    case App(funExpr, argExpr) =>
      App(subst(funExpr, substId, value), subst(argExpr, substId, value))
  }

  def interp(expr: FAE): FAE = expr match {
    case Num(n) => expr
    case Fun(arg, body) => expr
    case Id(name) => sys.error("found unbound id " + name)
    case Add(lhs, rhs) => Num(fromNum(interp(lhs)) + fromNum(interp(rhs)))
    case Sub(lhs, rhs) => Num(fromNum(interp(lhs)) - fromNum(interp(rhs)))
    case App(funExpr, argExpr) => interp(funExpr) match {
      case Fun(param, body) => interp(subst(body, param, interp(argExpr)))
      case _ => sys.error("Can only handle function expressions")
    }
  }

  def fromNum(expr: FAE): Int = expr match {
    case n: Num  => n.n
    case _ => sys.error("can only handle numbers")
  }

}