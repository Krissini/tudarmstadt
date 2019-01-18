import Interpreter._

class InterpreterTest extends org.scalatest.FunSuite {

  val t: BooleanExpression = True
  val f: BooleanExpression = False

  test("eval true")  { assertResult(TrueValue) { interp(t) } }
  test("test false") { assertResult(FalseValue) { interp(f) } }

  test("negation of false") { assertResult(TrueValue) { interp(Not(f)) } }
  test("negation of true")  { assertResult(FalseValue) { interp(Not(t)) } }

  test("conjunction TT") { assertResult(TrueValue) { interp(And(t, t)) } }
  test("conjunction TF") { assertResult(FalseValue) { interp(And(t, f)) } }
  test("conjunction FT") { assertResult(FalseValue) { interp(And(f, t)) } }
  test("conjunction FF") { assertResult(FalseValue) { interp(And(f, f)) } }

  test("disjunction TT") { assertResult(TrueValue) { interp(Or(t, t)) } }
  test("disjunction TF") { assertResult(TrueValue) { interp(Or(t, f)) } }
  test("disjunction FT") { assertResult(TrueValue) { interp(Or(f, t)) } }
  test("disjunction FF") { assertResult(FalseValue) { interp(Or(f, f)) } }

  test("double negation")    { assertResult(TrueValue) { interp(Not(Not(t))) } }
  test("recursive not")      { assertResult(FalseValue) { interp(Not(And(t, t))) } }
  test("nested conjunction") { assertResult(TrueValue) { interp(And(Not(f), Not(f))) } }
  test("nested disjunction") { assertResult(FalseValue) { interp(Or(Not(t), Not(t))) } }

  test("CNF") { assertResult(TrueValue) { interp(And(Or(Not(f), f), Or(f, t))) } }
}
