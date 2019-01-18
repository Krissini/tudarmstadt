package task2_implementing_and_using_extended_F1LAE

import task2_implementing_and_using_extended_F1LAE.ExtendedF1LAE._

class ExtendedF1LAETest extends org.scalatest.FunSuite {
  val funDefs = Map('f -> FunDef('f, 'n, App('g, Add('n, 5))),
    'g -> FunDef('g, 'n, Sub('n, 1)))

  test("App") {
    assertResult(9) {
      interp(App('f, 5), funDefs)
    }
  }

  test("If0 1") {
    assertResult(1) {
      interp(If0(0, 1, 2), Map.empty)
    }
  }

  test("If0 2") {
    assertResult(2) {
      interp(If0(1, 1, 2), Map.empty)
    }
  }

  test("If0, Let and Sub") {
    assertResult(2) {
      interp(If0(Let('x, 3, Sub('x, 3)), Let('x, 2, 'x), Let('x, 5, 'x)), Map.empty)
    }
  }

  test("If0 and App") {
    assertResult(2) {
      interp(If0(App('f, 0), 1, 2), funDefs)
    }
  }

  test("Nested If0 and App") {
    assertResult(2) {
      interp(If0(App('f, If0(App('f, 0), 1, 2)), 1, 2), funDefs)
    }
  }

  test("If0 and Let") {
    assertResult(3) {
      interp(If0(0, Let('x, 3, 'x), Let('x, 4, 'x)), Map.empty)
    }
  }

  test("If0, App and Let 1") {
    assertResult(2) {
      interp(If0(App('f, If0(Let('x, 0, 'x), 1, 2)), 1, 2), funDefs)
    }
  }

  test("If0, App and Let 2") {
    assertResult(42) {
      interp(If0(App('g, If0(Let('x, 0, 'x), 1, 2)), 42, 0), funDefs)
    }
  }
}
