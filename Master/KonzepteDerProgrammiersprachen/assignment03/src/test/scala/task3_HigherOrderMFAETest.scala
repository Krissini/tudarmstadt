import definitions.MFAEBase._
import HigherOrderMFAE._
import org.scalatest.FunSuite

class HigherOrderMFAETTest extends FunSuite {

  // example
  test("1") { assertResult(MNum(3)) { interpMultiarg(MApp(exampleFun, List(1, 2))) } }
  test("2") { assertResult(MNum(42)) { interpMultiarg(MApp(exampleFun, List(40, 2))) } }

  // If you are unfamiliar with implementing unit tests in Scala,
  // the test cases from previous examples can be used as examples

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  // TODO your functions and tests:

}