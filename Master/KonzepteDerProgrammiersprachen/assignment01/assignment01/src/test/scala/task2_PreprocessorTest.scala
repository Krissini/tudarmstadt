import Preprocessor._

class PreprocessorTest extends org.scalatest.FunSuite {

  def imply(lhs: BooleanExpression, rhs: BooleanExpression) = Or(Not(lhs), rhs)
  def biImply(lhs: BooleanExpression, rhs: BooleanExpression) = And(Or(Not(lhs), rhs), Or(Not(rhs), lhs))

  val t: BooleanExpression = True
  val f: BooleanExpression = False

  test("imp")               { assertResult(imply(t, f))             { preproc(Imp(t, f)) } }
  test("nested imp lhs")    { assertResult(Or(Not(imply(f, t)), t)) { preproc(Imp(Imp(f, t), t)) } }
  test("nested imp rhs")    { assertResult(Or(Not(t), imply(t, t))) { preproc(Imp(t, Imp(t, t))) } }
  test("bi imp")            { assertResult(biImply(t, f))           { preproc(BiImp(t, f)) } }
  test("nested bi imp lhs") { assertResult(biImply(t, imply(f, t))) { preproc(BiImp(t, Imp(f, t))) } }
  test("nested bi imp rhs") { assertResult(biImply(imply(f, t), f)) { preproc(BiImp(Imp(f, t), f)) } }
  test("nested in not")     { assertResult(Not(imply(t, t)))        { preproc(Not(Imp(t, t))) } }
  test("nested in and lhs") { assertResult(And(imply(f, t), t))     { preproc(And(Imp(f, t), t)) } }
  test("nested in and rhs") { assertResult(And(t, imply(f, t)))     { preproc(And(t, Imp(f, t))) } }
  test("nested in or lhs")  { assertResult(Or(imply(f, t), t))      { preproc(Or(Imp(f, t), t)) } }
  test("nested in or rhs")  { assertResult(Or(t, imply(f, t)))      { preproc(Or(t, Imp(f, t))) } }
  test("true")              { assertResult(t)                       { preproc(t) } }
  test("false")             { assertResult(f)                       { preproc(f) } }

}
