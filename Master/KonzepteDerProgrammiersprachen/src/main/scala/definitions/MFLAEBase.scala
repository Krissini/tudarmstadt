package definitions

object MFLAEBase {

  /** MFLAE: multiple-argument functions, lets and arithmetic expressions */
  sealed abstract class MFLAE
  case class MLNum(n: Int) extends MFLAE
  case class MLAdd(lhs: MFLAE, rhs: MFLAE) extends MFLAE
  case class MLSub(lhs: MFLAE, rhs: MFLAE) extends MFLAE
  case class MLLet(name: Symbol, namedExpr: MFLAE, body: MFLAE) extends MFLAE
  case class MLId(name: Symbol) extends MFLAE
  case class MLFun(params: List[Symbol], body: MFLAE) extends MFLAE
  case class MLApp(funExpr: MFLAE, args: List[MFLAE]) extends MFLAE

}