import CallByReferenceArgumentsInterpreter._
import org.scalatest.FunSuite

class CallByReferenceArgumentsInterpreterTest extends FunSuite {
  val swap =
    Fun('x,
      Fun('y,
        Let('t, 'x,
          Seqn(
            Set('x, 'y),
            Set('y, 't)))))

  test("simple set") {
    val (result, _) = eval(
      Let('x, 5,
        Seqn(
          Set('x, 42),
          'x)))
    assertResult (NumV(42)) { result }
  }

  test("nested set") {
    val (result, _) = eval(
      Let('x, 5,
        Seqn(
          Set('x, Seqn(Set('x, 23), Add('x, 'x))),
          'x)))
    assertResult (NumV(46)) { result }
  }

  test("non-ref swap") {
    val (result, _) = eval(
      Let('swap, swap,
        Let('x, 23,
          Let('y, 42,
            Seqn(
              App(App('swap, 'x), 'y),
              'x)))))
    assertResult(NumV(23)) { result }
  }

  test("boxes as variable values") {
    val (result, _) = eval(
      Let('x, NewBox(3),
        Let('y, NewBox(5),
          Seqn(
            Set('y, 'x),
            Seqn(
              SetBox('y, 10),
              OpenBox('x))))))
    assertResult (NumV(10)) { result }
  }

  test("functions as variable values") {
    val (result, _) = eval(
      Let('x, Fun('x, Add('x, 1)),
        Let('y, Fun('y, Add('x, 5)),
          Seqn(
            Set('y, 'x),
            App('y, 3)))))
    assertResult (NumV(4)) { result }
  }

  test("side effects on right side of assignment") {
    val (result, _) = eval(
      Let('x, 0,
        Let('y, 0,
          Seqn(
            Set('x, Set('y, 5)),
            Add('x, 'y)))))
    assertResult (NumV(10)) { result }
  }

  test("error on Ref in non-arg/home/spilot/Uni/bachelors/src/SOMns position") {
    intercept[Exception] { eval(Let('x, 3, Ref('x))) }
  }

  test("ref swap") {
    val (result, _) = eval(
      Let('swap, swap,
        Let('x, 23,
          Let('y, 42,
            Seqn(
              App(App('swap, Ref('x)), Ref('y)),
              'x)))))
    assertResult(NumV(42)) { result }
  }

  test("using ref as r-value") {
    val (result, _) = eval(
      Let('y, 5,
        App(Fun('x, 'x), Ref('y))))
    assertResult (NumV(5)) { result }
  }

  test("function as reference argument") {
    val (result, _) = eval(
      Let('y, Fun('x, Add('x, 'x)),
        App(App(Fun('x, 'x), Ref('y)), Num(1))))
    assertResult (NumV(2)) { result }
  }

  test("evaluation of function part of application extends store") {
    val (result, _) = eval(
      Let('z, 4,
        App(Let('y, 3, Fun('x, Add('x, 'y))), Ref('z))))
    assertResult (NumV(7)) { result }
  }
}
