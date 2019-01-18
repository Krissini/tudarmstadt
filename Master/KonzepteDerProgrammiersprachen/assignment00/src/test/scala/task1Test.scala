import Task1._

class Test extends org.scalatest.FunSuite {

  val one = Succ(Zero)
  val two = Succ(Succ(Zero))
  val three = Succ(Succ(Succ(Zero)))
  val five = Succ(Succ(Succ(Succ(Succ(Zero)))))

  test("add one zero")  { assertResult (one)  { add(one, Zero)  } }
  test("add zero five") { assertResult (five) { add(Zero, five) } }
  test("add one one")   { assertResult (two)  { add(one, one)   } }
  test("add two three") { assertResult (five) { add(two, three) } }

}
