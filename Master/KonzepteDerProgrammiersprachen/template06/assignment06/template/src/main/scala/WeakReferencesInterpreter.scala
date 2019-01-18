object WeakReferencesInterpreter {
  trait Store[Val] {
    def malloc(stack: List[Map[Symbol, Int]], v: Val): (Int, Store[Val])
    def update(index: Int, v: Val): Store[Val]
    def lookup(index: Int): Val
    def free(index: Int): Store[Val]
  }

  class MarkAndSweepStore(maxSize: Int) extends Store[Val] {

    val memory = new scala.collection.mutable.ArraySeq[Val](maxSize)

    var free: Int = maxSize

    var nextFreeAddr: Int = 0

    def malloc(stack: List[Env], v: Val): (Location, Store[Val]) = {
      if (free <= 0) gc(stack)
      if (free <= 0) sys.error("out of memory")

      /* Here we find the next available location in memory via a while-
       * loop. In order to avoid maintaining a list of available spaces,
       *  let us assume that all boxes in SRCFLAE contain data
       *  (in constrast to null values).
       *
       * If we ensure the invariant that the variable `free` has always
       * the number of free memory space, then the following loop will
       * always halt. The nontermination situation will generate an out-
       * of-memory error and the program will abort.
       */

      while (memory(nextFreeAddr) != null) {
        nextFreeAddr += 1
        if (nextFreeAddr == maxSize) nextFreeAddr = 0
      }

      free -= 1
      update(nextFreeAddr, v)
      (nextFreeAddr, this)
    }

    def update(index: Location, v: Val): Store[Val] = {
      memory.update(index, v)
      this
    }

    def lookup(index: Int) = memory(index)

    def free(index: Int) = {
      free += 1
      memory(index) = null
      this
    }

    def allAddrInVal(v: Val, set: Set[Int] = Set()): Set[Int] = v match {
      case Box(a) => set + a
      case NumV(_) => set
      case Closure(f, body, env) => allAddrInEnv(env, set)
    }

    def allAddrInEnv(env: Env, set: Set[Int] = Set()): Set[Int] = {
      val envAddresses = env.values.toSet
      // combine addresses of environment
      // with addresses of values pointed to
      // by the addresses of the environment
      envAddresses.foldRight(set)((index, set) => {
        val set2 = set + index
        if (set == set2)
          set
        else
          allAddrInVal(memory(index), set2)
      })
    }

    def mark(seed: Set[Int]): Unit = {
      seed.foreach(i => memory(i).marked = true)
      val allAddresses = seed.flatMap(ad => allAddrInVal(memory(ad)))
      val newAddresses = allAddresses.filter(!memory(_).marked)
      if (newAddresses != Set.empty) {
        mark(newAddresses)
      }
    }

    def sweep(): Unit = {
      memory.indices.foreach(
        index => if (memory(index) == null) {
          /* No work needed on an empty memory cell */
        } else if (memory(index).marked) {
          /* Reset `marked` flag for the next gc */
          memory(index).marked = false
        } else {
          free += 1
          memory(index) = null
        })
    }

    def gc(stack: List[Env]): Unit = {
      println("\nSTARTING GC\nSTACK = " + stack + "\nSTORE = " + memory)
      mark(stack.map(index => allAddrInEnv(index)).fold(Set.empty)(_ union _))
      sweep()
      println("GC COMPLETE\nSTORE = " + memory +
        "\nNUMBER OF FREE SLOTS = " + free)
    }
  }

  sealed abstract class Expr
  case class Num(n: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr
  case class Let(name: Symbol, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: Symbol) extends Expr
  case class If0(test: Expr, posBody: Expr, negBody: Expr) extends Expr
  case class Fun(param: Symbol, body: Expr) extends Expr
  case class Rec(name: Symbol, namedExpr: Expr, body: Expr) extends Expr
  case class App(funExpr: Expr, argExpr: Expr) extends Expr
  case class Seqn(e1: Expr, e2: Expr) extends Expr
  case class SetId(id: Symbol, valueExpr: Expr) extends Expr
  case class NewBox(valExpr: Expr) extends Expr
  case class SetBox(boxExpr: Expr, valueExpr: Expr) extends Expr
  case class OpenBox(boxExpr: Expr) extends Expr
  case class WeakRef(e: Expr) extends Expr
  case class TryDeref(refExpr: Expr, fallbackExpr: Expr) extends Expr

  type Location = Int
  type Env = Map[Symbol, Location]


  /** Use this value to invalidate a WeakRefV */
  val INVALID_LOC: Location = -1

  /*
   * We equip our values with a mutable flag that is useful for
   * mark-and-sweep garbage collection. In real systems it is
   * implemented as a bit flag, or, if the so-called "tri-color
   * algorithm" is used, with two bit flags.
   */
  sealed abstract class Val(var marked: Boolean = false)
  case class NumV(n: Int) extends Val
  case class Closure(param: Symbol, body: Expr, env: Env) extends Val
  case class Box(location: Location) extends Val
  case class WeakRefV(loc: Location) extends Val

  /*
   * In our interpreter, the stack of environments is only implicitly
   * available on the stack of the meta-language. To reify the call-
   * stack we need to make it explicit. We do so by constructing the
   * stack explicitly and passing it as parameter. The first element
   * of the stack is the current environment; the rest is only needed
   * for gc.
   */
  def interp(
    expr: Expr,
    stack: List[Env] = List(Map()),
    store: Store[Val] = new MarkAndSweepStore(100)): (Val, Store[Val]) = expr match {

    case Num(n) => (NumV(n), store)

    case Add(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 + n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }
    case Mult(lhs, rhs) => {
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match {
        case (NumV(n1), _) => {
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match {
            case (NumV(n2), _) => (NumV(n1 * n2), s2)
            case _ => sys.error(
              "can only add numbers, but got: %s and %s".format(lhsv, rhsv))
          }
        }
        case _ => sys.error(
          "can only add numbers, but got: '%s' as left hand side".format(lhsv))
      }
    }

    case Let(boundId, namedExpr, boundBody) => {
      val (namedVal, s1) = interp(namedExpr, stack, store)
      val (newLoc, s2) = s1.malloc(stack, namedVal)
      interp(boundBody, stack.head + (boundId -> newLoc) :: stack.tail, s2)
    }

    case Id(name) => (store.lookup(stack.head(name)), store)

    case Fun(arg, body) => (Closure(arg, body, stack.head), store)

    case If0(testExpr, thenExpr, elseExpr) => {
      val (testV, s1) = interp(testExpr, stack, store)
      testV match {
        case NumV(n) => {
          if (n == 0) interp(thenExpr, stack, s1)
          else interp(elseExpr, stack, s1)
        }
        case _ => sys.error("can only test numbers, but got: " + testV)
      }
    }

    /**
     * In our stateful language, we do not require mutation from the
     * host language to implement cyclic environments.
     */
    case Rec(boundId, namedExpr, boundBody) => {
      val (newLoc, s2) = store.malloc(stack, NumV(0))
      val extStack = Map(boundId -> newLoc) :: stack
      val (namedVal, bodyStore) = interp(namedExpr, extStack, store)
      interp(boundBody, extStack, bodyStore.update(newLoc, namedVal))
    }

    case App(funExpr, argExpr) => {
      val (funV, funStore) = interp(funExpr, stack, store)
      val (argV, argStore) = interp(argExpr, stack, funStore)
      funV match {
        case Closure(fParam, fBody, fEnv) => {
          val (newLoc, resStore) = argStore.malloc(stack, argV)
          interp(fBody, fEnv + (fParam -> newLoc) :: stack, resStore)
        }
        case _ => sys.error("can only apply functions, but got: " + funV)
      }
    }

    case Seqn(e1, e2) => {
      val (v1, s1) = interp(e1, stack, store)
      interp(e2, stack, s1)
    }

    case NewBox(boxExpr) => {
      val (boxV, boxStore) = interp(boxExpr, stack, store)
      val (newLoc, resStore) = boxStore.malloc(stack, boxV)
      (Box(newLoc), resStore)
    }

    case SetBox(boxExpr, valueExpr) => {
      val (boxV, s1) = interp(boxExpr, stack, store)
      val (value, s2) = interp(valueExpr, stack, s1)
      boxV match {
        case Box(loc) => (value, s2.update(loc, value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
      }
    }

    case OpenBox(boxExpr) => {
      val (boxV, s1) = interp(boxExpr, stack, store)
      boxV match {
        case Box(loc) => (s1.lookup(loc), s1)
        case _ => sys.error("can only open boxes, but got: " + boxV)
      }
    }

    case SetId(id, valExpr) => {
      val (value, s1) = interp(valExpr, stack, store)
      (value, s1.update(stack.head(id), value))
    }
  }
}
