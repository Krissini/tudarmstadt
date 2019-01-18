import scala.language.implicitConversions
import definitions.FLAEBase._
import definitions.MFLAEBase._
import org.scalatest.FunSuite
import PreprocMultiarg._

class PreprocMultiargTest extends FunSuite {

  implicit def intToMFLAE(n: Int): MFLAE = MLNum(n)
  implicit def intToFLAE(n: Int): FLAE = LNum(n)
  implicit def symbolToMFLAE(s: Symbol): MFLAE = MLId(s)
  implicit def symbolToFLAE(s: Symbol): FLAE = LId(s)

  val testData: Seq[(String, FLAE, FLAE)] = Seq(
    // (testname,
    //  expected,
    //  actual),

    ("preproc fun zero arg",
      LFun('_, 3), // dummy argument '_
      preprocMultiarg(MLFun(List(), 3))),
    ("preproc app zero arg",
      LApp(LFun('_, 3), LNum(42)), // dummy value 42
      preprocMultiarg(MLApp(MLFun(List(), 3), List()))),

    ("interp fun zero arg",
      LFun('_, LNum(3)),
      interpLet(preprocMultiarg(MLLet('c, MLFun(List(), 3), 'c)))),
    ("interp app zero arg",
      LNum(3),
      interpLet(preprocMultiarg(MLLet('c, MLFun(List(), 3), MLApp('c, List()))))),

    ("preproc fun two arg",
      LFun('a, LFun('b, LSub('a, 'b))),
      preprocMultiarg(MLFun(List('a, 'b), MLSub('a, 'b)))),
    ("preproc app two arg",
      LApp(LApp('f, 'a), 'b),
      preprocMultiarg(MLApp('f, List('a, 'b)))),
    ("preproc app fun two arg",
      LApp(LApp(LFun('a, LFun('b, LSub('a, 'b))), 1), 2),
      preprocMultiarg(MLApp(MLFun(List('a, 'b), MLSub('a, 'b)), List(1, 2)))),

    ("fun three arg order",
      LFun('a, LFun('b, LFun('c, 'body))),
      preprocMultiarg(MLFun(List('a, 'b, 'c), 'body))),
    ("app three arg order",
      LApp(LApp(LApp('f, 'a), 'b), 'c),
      preprocMultiarg(MLApp('f, List('a, 'b, 'c)))),

    ("interp 1",
      LNum(-1),
      interpLet(preprocMultiarg(MLApp(MLFun(List('a, 'b), MLSub(1, 2)), List(1, 2))))),
    ("interp 2",
      LNum(-1),
      interpLet(preprocMultiarg(MLLet('f, MLFun(List('a, 'b), MLSub('a, 'b)), MLApp('f, List(1, 2)))))),

    ("advanced test 1",
      LNum(1),
      interpLet(preprocMultiarg(
        MLLet('f, MLFun(List('x, 'y), MLSub('x, 'y)),
          MLLet('y, 2, MLApp('f, List('y, 1))))))),
    ("advanced test 2",
      LNum(3),
      interpLet(preprocMultiarg(
        MLLet('f, MLFun(List('x, 'y), MLAdd('x, 'y)),
          MLApp('f, List(MLLet('x, 2, MLSub('x, 1)), 2)))))),
    ("advanced test 3",
      LNum(2),
      interpLet(preprocMultiarg(
        MLLet('f, MLFun(List('x, 'y, 'z), MLAdd('x, 'z)),
          MLApp('f, List(MLLet('a, 1, MLSub('a, 1)), 3, 2))))))
  )

  testData.foreach((testDatum) =>
    test(testDatum._1) { assertResult(testDatum._2) { testDatum._3 } })

}
