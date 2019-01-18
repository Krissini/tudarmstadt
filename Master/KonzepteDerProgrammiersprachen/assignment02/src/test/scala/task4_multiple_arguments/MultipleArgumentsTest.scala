package task4_multiple_arguments

import task4_multiple_arguments.MultipleArguments._

class MultipleArgumentsTest extends org.scalatest.FunSuite {
  val funDefs = Map('f -> FunDef('f, List('n), App('g, List(Add('n, 5)))),
    'g -> FunDef('g, List('n), Sub('n, 1)))

  test("interp app0") {
    assertResult(2) {
      interp(App('c, List()), Map('c -> FunDef('c, List(), 2)))
    }
  }

  test("interp app1") {
    assertResult(9) {
      interp(App('f, List(5)), funDefs)
    }
  }


  test("interp app2") {
    assertResult(3) {
      interp(App('f, List(1, 2)), Map('f -> FunDef('f, List('x, 'y), Add('x, 'y))))
    }
  }

  test("interp app3") {
    assertResult(0) {
      interp(App('g, List(1, 2, 3)), Map('g -> FunDef('g, List('x, 'y, 'z), Add('x, Sub('y, 'z)))))
    }
  }

  test("interp let") {
    assertResult(2) {
      interp(Let('y, 2, 'y), Map.empty[Symbol, FunDef])
    }
  }

  test("interp let app2") {
    assertResult(1) {
      interp(Let('y, 2, App('f, List('y, 1))), Map('f -> FunDef('f, List('x, 'y), Sub('x, 'y))))
    }
  }

  test("interp let let app2") {
    assertResult(1) {
      interp(Let('y, 2, Let('x, 1, App('f, List('y, 'x)))), Map('f -> FunDef('f, List('x, 'y), Sub('x, 'y))))
    }
  }

  test("interp app, expr in arg") {
    assertResult(3) {
      interp(App('f, List(Let('x, 2, Sub('x, 1)), 2)), Map('f -> FunDef('f, List('x, 'y), Add('x, 'y))))
    }
  }

  test("interp app1, let in add") {
    assertResult(7) {
      interp(App('f, List(5)), Map('f -> FunDef('f, List('x), Add(Let('x, 2, 'x), 'x))))
    }
  }

  test("interp app2, let in add, 1") {
    assertResult(4) {
      interp(App('f, List(1, 2)), Map('f -> FunDef('f, List('x, 'y), Add(Let('x, 2, 'x), 'y))))
    }
  }

  test("interp app2, let in add, 2") {
    assertResult(4) {
      interp(App('f, List(5, 2)), Map('f -> FunDef('f, List('x, 'y), Add(Let('x, 2, 'x), 'y))))
    }
  }

  test("interp complex") {
    assertResult(42) {
      interp(
        Let('y, App('A, List()),
        Let('x, Sub('y, 25),
        App('f, List('x, Sub('y, App('f, List(Sub(App('f, List('x, 1)), 1), App('f, List(20, 3))))))))),
        Map(
          'f -> FunDef('f, List('x, 'y), Add('x, 'y)),
          'A -> FunDef('A, List(), 65)))
    }
  }

}
