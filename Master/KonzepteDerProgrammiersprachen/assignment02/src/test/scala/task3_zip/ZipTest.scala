package task3_zip

import task3_zip.Zip._

class ZipTest extends org.scalatest.FunSuite {
  test("Zipadd0") {
    assertResult(List(5, 7, 9)) {
      zipadd(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("Zipadd1") {
    assertResult(List(17, 12, 10, 8, 7)) {
      zipadd(List(9, 8, 4, 3, 6), List(8, 4, 6, 5, 1))
    }
  }

  test("Zipadd2") {
    assertResult(List(17, 12, 10)) {
      zipadd(List(9, 8, 4, 3, 6), List(8, 4, 6))
    }
  }

  test("Zipadd3") {
    assertResult(List(17, 12)) {
      zipadd(List(9, 8), List(8, 4, 6, 5, 1))
    }
  }

  test("Zip0") {
    assertResult(List((1, 4), (2, 5), (3, 6))) {
      zip(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("Zip1") {
    assertResult(List((9, 8), (8, 4), (4, 6), (3, 5), (6, 1))) {
      zip(List(9, 8, 4, 3, 6), List(8, 4, 6, 5, 1))
    }
  }

  test("Zip2") {
    assertResult(List((9, 8), (8, 4), (4, 6), (3, 5))) {
      zip(List(9, 8, 4, 3, 6), List(8, 4, 6, 5))
    }
  }

  test("Zip3") {
    assertResult(List((9, 8))) {
      zip(List(9), List(8, 4, 6, 5, 1))
    }
  }

  test("AddMatrix0") {
    assertResult(List(List(8, 10), List(13, 15))) {
      addMatrix(List(List(1, 2), List(4, 5)), List(List(7, 8), List(9, 10)))
    }
  }

  test("AddMatrix1") {
    assertResult(List(List(10, 11, 10), List(4, 13, 14), List(11, 12, 6))) {
      addMatrix(List(List(9, 8, 4), List(3, 6, 8), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6), List(7, 6, 1)))
    }
  }

  test("AddMatrix2") {
    assertResult(List(List(10, 11, 10), List(4, 13), List(11))) {
      addMatrix(List(List(9, 8, 4), List(3, 6), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6), List(7)))
    }
  }

  test("AddMatrix3") {
    assertResult(List(List(10), List(4, 13, 14))) {
      addMatrix(List(List(9), List(3, 6, 8), List(4, 6, 5)), List(List(1, 3, 6), List(1, 7, 6)))
    }
  }
}
