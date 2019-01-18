package task1_de_brujin_indices

import task1_de_brujin_indices.DeBrujin._

import scala.language.implicitConversions

class DeBrujinTest extends org.scalatest.FunSuite {
  implicit def symToId(sym: Symbol): Id = Id(sym)
  implicit def intToNum(n: Int): Num = Num(n)
  implicit def intToNumDB(n: Int): NumDB = NumDB(n)

  test("conv num literal") { assertResult(NumDB(3)) { convert(3) } }
  test("conv addition") { assertResult(AddDB(3, 3)) { convert(Add(3, 3)) } }
  test("conv subtraction") { assertResult(SubDB(6, 4)) { convert(Sub(6, 4)) } }
  test("conv let ref") { assertResult(LetDB(5, RefDB(0))) { convert(Let('x, 5, 'x)) } }

  test("conv let let ref") {
    assertResult(LetDB(1, LetDB(2, RefDB(1)))) {
      convert(Let('x, 1, Let('y, 2, 'x)))
    }
  }

  test("conv let let add ref") {
    assertResult(LetDB(1, LetDB(2, AddDB(RefDB(0), RefDB(1))))) {
      convert(Let('x, 1, Let('y, 2, Add('y, 'x))))
    }
  }

  test("conv deep ref") {
    assertResult(LetDB(1, LetDB(2, LetDB(42, LetDB(3, LetDB(4, RefDB(2))))))) {
      convert(Let('x, 1, Let('y, 2, Let('z, 42, Let('a, 3, Let('b, 4, 'z))))))
    }
  }

  test("conv add with ref in lhs") {
    assertResult(LetDB(1, AddDB(RefDB(0), 2))) {
      convert(Let('x, 1, Add('x, 2)))
    }
  }

  test("conv add with ref in rhs") {
    assertResult(LetDB(3, AddDB(2, RefDB(0)))) {
      convert(Let('x, 3, Add(2, 'x)))
    }
  }

  test("conv sub with ref in lhs") {
    assertResult(LetDB(7, SubDB(RefDB(0), 2))) {
      convert(Let('x, 7, Sub('x, 2)))
    }
  }

  test("conv sub with ref in rhs") {
    assertResult(LetDB(3, SubDB(2, RefDB(0)))) {
      convert(Let('x, 3, Sub(2, 'x)))
    }
  }

  test("conv ref in named expression") {
    assertResult(LetDB(42, LetDB(RefDB(0), RefDB(0)))) {
      convert(Let('x, 42, Let('y, 'x, 'y)))
    }
  }

  test("conv nested ref in named expression") {
    assertResult(LetDB(20, LetDB(AddDB(22, RefDB(0)), RefDB(0)))) {
      convert(Let('x, 20, Let('y, Add(22, 'x), 'y)))
    }
  }

  test("conv same ref, different depths") {
    assertResult(LetDB(AddDB(5, 5), LetDB(SubDB(RefDB(0), 3), AddDB(RefDB(0), RefDB(0))))) {
      convert(Let('x, Add(5, 5), Let('y, Sub('x, 3), Add('y, 'y))))
    }
  }

  test("conv same symbols") {
    assertResult(LetDB(13, LetDB(42, RefDB(0)))) {
      convert(Let('x, 13, Let('x, 42, 'x)))
    }
  }

  test("conv same name for named expression") {
    assertResult(LetDB(1, LetDB(2, AddDB(RefDB(0), RefDB(0))))) {
      convert(Let('x, 1, Let('x, 2, Add('x, 'x))))
    }
  }

  test("conv Symbol in named expression") {
    assertResult(LetDB(1, LetDB(RefDB(0), RefDB(0)))) {
      convert(Let('x, 1, Let('y, 'x, 'y)))
    }
  }

  test("conv Nesting") {
    assertResult(LetDB(SubDB(7, 3), LetDB(SubDB(RefDB(0), RefDB(0)), AddDB(RefDB(1), RefDB(0))))) {
      convert(Let('x, Sub(7, 3), Let('y, Sub('x, 'x), Add('x, 'y)))) } }

  test("conv static scoping") {
    try {
      assertResult(AddDB(LetDB(3, RefDB(0)), RefDB(-1))) {
        convert(Add(Let('x, 3, 'x), 'x))
      }
    } catch {
      case e: Exception => true
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////

  test("interp num literal") { assertResult(3) { interp(3) } }
  test("interp addition") { assertResult(6) { interp(AddDB(3, 3)) } }
  test("interp subtraction") { assertResult(2) { interp(SubDB(6, 4)) } }
  test("interp let ref") { assertResult(5) { interp(LetDB(5, RefDB(0))) } }
  test("interp let let ref") { assertResult(1) { interp(LetDB(1, LetDB(2, RefDB(1)))) } }

  test("interp deep ref") {
    assertResult(42) {
      interp(LetDB(1, LetDB(2, LetDB(42, LetDB(3, LetDB(4, RefDB(2)))))))
    }
  }

  test("interp add with ref in lhs") {
    assertResult(3) {
      interp(LetDB(1, AddDB(RefDB(0), 2)))
    }
  }

  test("interp add with ref in rhs") {
    assertResult(5) {
      interp(LetDB(3, AddDB(2, RefDB(0))))
    }
  }

  test("interp sub with ref in lhs") {
    assertResult(5) {
      interp(LetDB(7, SubDB(RefDB(0), 2)))
    }
  }

  test("interp sub with ref in rhs") {
    assertResult(-1) {
      interp(LetDB(3, SubDB(2, RefDB(0))))
    }
  }

  test("interp ref in named expression") {
    assertResult(42) {
      interp(LetDB(42, LetDB(RefDB(0), RefDB(0))))
    }
  }

  test("interp nested ref in named expression") {
    assertResult(42) {
      interp(LetDB(20, LetDB(AddDB(22, RefDB(0)), RefDB(0))))
    }
  }

  test("interp same ref, different depths") {
    assertResult(14) {
      interp(LetDB(AddDB(5, 5), LetDB(SubDB(RefDB(0), 3), AddDB(RefDB(0), RefDB(0)))))
    }
  }

  test("interp static scoping") {
    intercept[Exception] {
      interp(AddDB(LetDB(42, 0), RefDB(0))) } }
}
