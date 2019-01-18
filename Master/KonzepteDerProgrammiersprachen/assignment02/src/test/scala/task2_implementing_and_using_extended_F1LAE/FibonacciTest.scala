package task2_implementing_and_using_extended_F1LAE

import task2_implementing_and_using_extended_F1LAE.ExtendedF1LAE._
import task2_implementing_and_using_extended_F1LAE.Fibonacci._

class FibonacciTest extends org.scalatest.FunSuite {
  test("test0") {
    assertResult(0) {
      interp(App('fib, 0), fibDefs)
    }
  }

  test("test1") {
    assertResult(1) {
      interp(App('fib, 1), fibDefs)
    }
  }

  test("test2") {
    assertResult(1) {
      interp(App('fib, 2), fibDefs)
    }
  }

  test("test3") {
    assertResult(2) {
      interp(App('fib, 3), fibDefs)
    }
  }

  test("test4") {
    assertResult(3) {
      interp(App('fib, 4), fibDefs)
    }
  }

  test("test5") {
    assertResult(5) {
      interp(App('fib, 5), fibDefs)
    }
  }

  test("test6") {
    assertResult(8) {
      interp(App('fib, 6), fibDefs)
    }
  }

  test("test7") {
    assertResult(13) {
      interp(App('fib, Let('x, 6, Add('x, 1))), fibDefs)
    }
  }

  test("test8") {
    assertResult(21) {
      interp(App('fib, Let('x, 6, Add('x, 2))), fibDefs)
    }
  }

  test("test9") {
    assertResult(34) {
      interp(App('fib, Let('x, 7, Add(2, 'x))), fibDefs)
    }
  }

  def test10 {
    assertResult(55) {
      interp(App('fib, Add(4, 6)), fibDefs)
    }
  }

  def test11 {
    assertResult(89) {
      interp(Add(App('fib, 9), App('fib, 10)), fibDefs)
    }
  }

  // TODO?
  /*
  def test20 {
    assertResult (6765) {
      interp(App('fib, 20), fibDefs)
    }
  }
  */
}
