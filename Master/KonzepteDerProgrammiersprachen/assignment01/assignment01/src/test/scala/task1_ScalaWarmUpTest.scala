import ScalaWarmUp._

class ScalaWarmUpTest extends org.scalatest.FunSuite {

  test("flatten1") { assertResult (List(1, 2, 3, 4, 5, 6)) { flatten(List(List(1), List(2, 3, 4), List(5, 6))) } }
  test("flatten2") { assertResult (Nil) { flatten(Nil) } }
  test("flatten3") { assertResult (Nil) { flatten(List(Nil)) } }
  test("flatten4") { assertResult (List(1,2,3)) { flatten(List(Nil, List(1), Nil, List(2), Nil, List(3), Nil, Nil, Nil)) } }

  test("nil")       { assertResult (Nil) { reverse(Nil) } }
  test("singleton") { assertResult (List(1)) { reverse(List(1)) } }
  test("reverse")   { assertResult (List(3,2,1)) { reverse(List(1, 2, 3)) } }
  test("dont use flatten for reverse") {
    assertResult (List(List(3,4), List(1,2))) { reverse(List(List(1,2), List(3,4))) }
  }

}
