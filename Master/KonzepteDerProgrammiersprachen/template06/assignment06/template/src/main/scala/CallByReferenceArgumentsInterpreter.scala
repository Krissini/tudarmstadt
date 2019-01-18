object CallByReferenceArgumentsInterpreter {
  sealed abstract class SCFLAE
  case class Num(n: Int) extends SCFLAE
  case class Add(lhs: SCFLAE, rhs: SCFLAE) extends SCFLAE
  case class Let(name: Symbol, namedExpr: SCFLAE, body: SCFLAE) extends SCFLAE
  case class Id(name: Symbol) extends SCFLAE
  case class If0(test: SCFLAE, posBody: SCFLAE, negBody: SCFLAE) extends SCFLAE
  case class Fun(param: Symbol, body: SCFLAE) extends SCFLAE
  case class App(funExpr: SCFLAE, argExpr: SCFLAE) extends SCFLAE
  case class Seqn(e1: SCFLAE, e2: SCFLAE) extends SCFLAE
  case class Set(id: Symbol, valueExpr: SCFLAE) extends SCFLAE
  case class NewBox(valExpr: SCFLAE) extends SCFLAE
  case class SetBox(boxExpr: SCFLAE, valueExpr: SCFLAE) extends SCFLAE
  case class OpenBox(boxExpr: SCFLAE) extends SCFLAE
  case class Ref(name: Symbol) extends SCFLAE

  implicit def idToSCFLAE(id: Symbol) = Id(id)
  implicit def numToSCFLAE(n: Int) = Num(n)

  type Location = Int
  var _currentLocation = 0
  def nextLocation: Location = {
    _currentLocation += 1
    _currentLocation
  }

  type Env = Map[Symbol, Location]
  type Store = Map[Location, Val]

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: Symbol, body: SCFLAE, env: Env) extends Val
  case class Box(location: Location) extends Val

  def eval(expr: SCFLAE) = interp(expr, Map(), Map())

  def interp(expr: SCFLAE, env: Env, store: Store): (Val, Store) = expr match {
    case Num(n) => (NumV(n), store)

    case Add(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, env, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, env, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 + n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error("can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    case Let(boundId, namedExpr, boundBody) => {
      val (namedVal, s1) = interp(namedExpr, env, store)
      val newLoc = nextLocation
      interp(boundBody, env + (boundId -> newLoc), s1 + (newLoc -> namedVal))
    }

    case Id(name) => (store(env(name)), store)

    case Fun(arg, body) => (Closure(arg, body, env), store)

    case If0(testExpr, thenExpr, elseExpr) => {
      val (testV, s1) = interp(testExpr, env, store)
      testV match {
        case NumV(n) => {
          if (n == 0) interp(thenExpr, env, s1)
          else interp(elseExpr, env, s1)
        }
        case _ => sys.error("can only test numbers, but got: " + testV)
      }
    }

    /*
     * call-by-reference:
     * A reference to (the location of the) actual argument (not its value) is passed when a function is called.
     * Updates to the reference (location) within the called function become visible to the calling context.
     * (+) less allocation to the store
     * (-) creates aliases and makes reasoning about programs harder
     */
    case App(funExpr, Ref(argName)) => {/*
      val (funV, funStore) = interp(funExpr, env, store)
      funV match {
        case RefClosure(fParam, fBody, fEnv) => Ref(argName) match {
          case Id(argName) => {
            interp(fBody, fEnv + (fParam -> env(argName)), funStore)
          }
          case _ => sys.error("Expressions of type Ref(identifier) only need to be supported as wrapper for arguments")
        }
        case _ => sys.error("Expressions of type Ref(identifier) only need to be supported as wrapper for arguments")
      }*/
    }

    case App(funExpr, argExpr) => {
      val (funV, funStore) = interp(funExpr, env, store)
      val (argV, argStore) = interp(argExpr, env, funStore)
      funV match {
        case Closure(fParam, fBody, fEnv) => {
          val newLoc = nextLocation
          interp(fBody, fEnv + (fParam -> newLoc), argStore + (newLoc -> argV))
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }

    case Seqn(e1, e2) => {
      val (v1, s1) = interp(e1, env, store)
      interp(e2, env, s1)
    }

    case NewBox(boxExpr) => {
      val (boxV, boxStore) = interp(boxExpr, env, store)
      val newLoc = nextLocation
      (Box(newLoc), boxStore + (newLoc -> boxV))
    }

    case SetBox(boxExpr, valueExpr) => {
      val (boxV, s1) = interp(boxExpr, env, store)
      val (value, s2) = interp(valueExpr, env, s1)
      boxV match {
        case Box(loc) => (value, s2 + (loc -> value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
      }
    }

    case OpenBox(boxExpr) => {
      val (boxV, s1) = interp(boxExpr, env, store)
      boxV match {
        case Box(loc) => (s1(loc), s1)
        case _ => sys.error("can only open boxes, but got: " + boxV)
      }
    }

    case Set(id, valExpr) => {
      val (value, s1) = interp(valExpr, env, store)
      (value, s1 + (env(id) -> value))
    }
  }
}
