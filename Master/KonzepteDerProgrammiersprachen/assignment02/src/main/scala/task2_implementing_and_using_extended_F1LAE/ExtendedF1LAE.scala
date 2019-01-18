package task2_implementing_and_using_extended_F1LAE

//import task2_implementing_and_using_extended_F1LAE.ExtendedF1LAESolution.F1LAE

import scala.language.implicitConversions

object ExtendedF1LAE {
  // AST for expressions of F1LAE
  sealed abstract class F1LAE
  case class Num(n: Int) extends F1LAE
  case class Add(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Sub(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Let(name: Symbol, namedExpr: F1LAE, body: F1LAE) extends F1LAE
  case class Id(name: Symbol) extends F1LAE
  case class App(funName: Symbol, arg: F1LAE) extends F1LAE
  case class If0(test: F1LAE, thenBody: F1LAE, elseBody: F1LAE) extends F1LAE

  // AST for function definition forms in F1LAE
  case class FunDef(funName: Symbol, argName: Symbol, body: F1LAE)
  type FunDefs = Map[Symbol, FunDef]

  implicit def symbolToExpr(symbol: Symbol): Id = Id(symbol)
  implicit def intToExpr(n: Int): Num = Num(n)

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // evaluates F1LAE expressions by reducing them to their corresponding values
  def interp(expr: F1LAE, funDefs: FunDefs): Int = expr match {
    case Num(n) => n
    case Add(lhs, rhs) => interp(lhs, funDefs) + interp(rhs, funDefs)
    case Sub(lhs, rhs) => interp(lhs, funDefs) - interp(rhs, funDefs)
    case Id(name) => sys.error("found unbound id " + name)
    case Let(boundId, namedExpr, boundExpr) =>
      interp(subst(boundExpr, boundId, Num(interp(namedExpr, funDefs))), funDefs)
    case App(funName, argExpr) => funDefs(funName) match {
      case FunDef(funName, argName, body) =>
        interp(subst(body, argName, interp(argExpr, funDefs)), funDefs)
    }
  }

  // substitutes 2nd argument with 3rd argument in 1st argument. The resulting
  // expression contains no free instances of the 2nd argument.
  def subst(expr: F1LAE, substId: Symbol, value: F1LAE): F1LAE = expr match {
    case Num(n) => expr
    case Add(lhs, rhs) => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Id(name) => if (substId == name) value else expr
    case Let(boundId, namedExpr, boundExpr) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if (boundId == substId)
        Let(boundId, substNamedExpr, boundExpr)
      else
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))
    case App(funName, argExpr) => App(funName, subst(argExpr, substId, value))
  }
}
