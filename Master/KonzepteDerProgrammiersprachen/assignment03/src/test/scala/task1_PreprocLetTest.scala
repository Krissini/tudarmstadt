import scala.language.implicitConversions
import definitions.FAEBase._
import definitions.FLAEBase._
import org.scalatest.FunSuite
import PreprocLet._

class PreprocLetTest extends FunSuite {

  // implicitly convert
  //   numbers to LNums or Nums, and
  //   Symbols to LIds or Ids
  implicit def numToFLAE(n: Int): LNum = LNum(n)
  implicit def numToFAE(n: Int): Num = Num(n)
  implicit def symbolToFLAE(s: Symbol): LId = LId(s)
  implicit def symbolToFAE(s: Symbol): Id = Id(s)

  test("num") { assertResult (Num(3)) { preprocLet(LNum(3)) } }
  test("id")  { assertResult (Id('x)) { preprocLet(LId('x)) } }
  test("fun") { assertResult (Fun('x, Id('x))) { preprocLet(LFun('x, LId('x))) } }
  test("let") { assertResult (App(Fun('x, Id('x)), Num(3))) { preprocLet(LLet('x, LNum(3), 'x)) } }

  test("add") {
    assertResult (Add(Add(Num(3), Num(4)), Add(Num(6), Num(7)))) {
      preprocLet(LAdd(LAdd(LNum(3), LNum(4)), LAdd(LNum(6), LNum(7))))
    }
  }

  test("sub") {
    assertResult (Sub(Sub(Num(3), Num(4)), Sub(Num(6), Num(7)))) {
      preprocLet(LSub(LSub(LNum(3), LNum(4)), LSub(LNum(6), LNum(7))))
    }
  }

  test("let fun") {
    assertResult (Fun('y, Add(3, 'y))) {
      interp(preprocLet(LLet('x, 3, LFun('y, LAdd('x, 'y)))))
    }
  }

  test("fun app") {
    assertResult (Num(12)) {
      interp(preprocLet(LApp(LLet('x, 3, LFun('y, LAdd('x, 'y))), LAdd(4, 5))))
    }
  }

  test("let fun app") {
    assertResult (Num(11)) {
      interp(preprocLet(LLet('inc,
        LFun('x, LAdd('x, 1)),
        LAdd(LApp('inc, 4), LApp('inc, 5)))))
    }
  }

  // You may write additional tests if you want... :

}
