import org.scalatest.FunSuite
import WeakReferencesInterpreter._

class WeakReferencesInterpreterTest extends FunSuite {
  implicit def idToSCFLAE(id: Symbol) = Id(id)
  implicit def numToSCFLAE(n: Int) = Num(n)

  val emptyEnv: Env = Map()
  val emptyStack: List[Env] = emptyEnv :: Nil

  test("correct WeakRef") {
    val (res, st) = interp(Let('ref, WeakRef(42), 'ref))
    assert(st.lookup(0) === NumV(42))
    assert(st.lookup(1) === WeakRefV(0))
  }

  test("invalidates location for WeakRefV on gc") {
    val st = new MarkAndSweepStore(2)
    val (targetLoc, _) = st.malloc(emptyStack, NumV(42))
    val (refLoc, _) = st.malloc(emptyStack, WeakRefV(targetLoc))
    st.gc(List(Map('ref -> refLoc)))
    assert(st.lookup(refLoc) === WeakRefV(INVALID_LOC), "WeakRefV location must be set to %s on gc".format(INVALID_LOC))
  }

  test("WeakRef and successful Deref") {
    assertResult(NumV(42)) {
      val (res, _) = interp(Let('ref, WeakRef(42), TryDeref('ref, 23)))
      res
    }
  }

  test("WeakRef and unsuccessful Deref") {
    assertResult(NumV(23)) {
      val st = new MarkAndSweepStore(2)
      val (res, _) = interp(
        Let('ref, WeakRef(42),
          Seqn(Let('x, -1, 'x), TryDeref('ref, 23))),
          emptyStack,
          st)
      res
    }
  }


  test("Nested WeakRefs") {
    assertResult((NumV(23), 1)) {
      val st = new MarkAndSweepStore(3)
      val (res, sRes) = interp(Let('ref, WeakRef(WeakRef(42)),
          Seqn(Let('x, -1, 'x), TryDeref('ref, 23))),
          emptyStack,
          st)
      (res, st.free)
    }
  }

  test("GC WeakRef itself and referenced value") {
    assertResult(NumV(-1)) {
      val st = new MarkAndSweepStore(2)
      val (res, _) = interp(
        Seqn(Let('ref, WeakRef(42), Num(0)), Let('x, -1, 'x)),
          emptyStack,
          st)
      res
    }
  }

  // This test case checks if multiple runs of GC still work as intended.
  // Notably, this is the only situation where forward references can occur in our language,
  // so the test case verifies this too.
  test("Multiple GCs and forward references") {
    assertResult(NumV(3)) {
      val st = new MarkAndSweepStore(4)
      val (res, _) = interp(
        Let('ref, WeakRef(42), Seqn(
          Let('ref2, WeakRef(23), Let('x, -1, 'x)),
          Let('a, 1, Let('b, 2, Let('c, 3, Add('a, 'b)))))),
          emptyStack,
          st)
      res
    }
    intercept[Exception] {
      val st = new MarkAndSweepStore(4)
      val (res, _) = interp(
        Let('ref, WeakRef(42), Seqn(
          Let('ref2, WeakRef(23), Let('x, -1, 'x)),
          Let('a, 1, Let('b, 2, Let('c, 3, Let('d, 4, Add('a, 'b))))))),
          emptyStack,
          st)
      res
    }
  }

    // It may not be obvious but this test case is specifically designed to fail if WeakRefs
    // are still referentially distinct after being set to INVALID_LOC, as if not, GC will "swallow"
    // 'ref2 when collecting the content of 'ref.
    test("no referential equality of invalidated WeakRefs") {
    assertResult(NumV(100)) {
      val st = new MarkAndSweepStore(4)
      val (res, _) = interp(
        Let('ref, WeakRef(42),
          Let('ref2, WeakRef(23), Seqn(
            Let('x, -1, 'x),
            TryDeref('ref2, Num(100))))),
          emptyStack,
          st)
      res
    }

  }


      test("NewBox and OpenBox") {
        assertResult(NumV(1)) {
          val (res, _) = interp(Let('a, NewBox(1), OpenBox('a)))
          res
        }
      }

      test("Fun and Seq") {
        assertResult(NumV(7)) {
          val (res, _) = interp(
            Let('a, NewBox(1),
              Let('f, Fun('x, Add('x, OpenBox('a))),
                Seqn(SetBox('a, 2), App('f, 5)))))
          res
        }
      }

      test("If0 & SetBox") {
        assertResult(NumV(1)) {
          val (res, _) = interp(
            Let('switch, NewBox(0),
              Let('toggle,
                Fun('dummy,
                  If0(OpenBox('switch),
                    Seqn(SetBox('switch, 1), 1),
                    Seqn(SetBox('switch, 0), 0))),
                Add(App('toggle, 42), App('toggle, 42)))))
          res
        }
      }

      test("If0 & SetId") {
        assertResult(NumV(1)) {
          val (res, _) = interp(
            Let('switch, 0,
              Let('toggle,
                Fun('dummy,
                  If0('switch,
                    Seqn(SetId('switch, 1), 1),
                    Seqn(SetId('switch, 0), 0))),
                Add(App('toggle, 42), App('toggle, 42)))))
          res
        }
      }

      test("Multiple Boxes") {
        assertResult(NumV(7)) {
          val (res, _) = interp(
            App(Fun('b1, App(Fun('b2, Seqn(SetBox('b1, 6), OpenBox('b2))), NewBox(7))),
              NewBox(5)))
          res
        }
      }

      test("Correct Seqn return value") {
        assertResult(NumV(5)) {
          val (res, _) = interp(
            Let('b, 0,
              If0(Seqn(SetId('b, 5), 'b),
                1,
                'b)))
          res
        }
      }

      test("test7") {
        assertResult(NumV(9)) {
          val (res, _) = interp(Let('b, 4, Add('b, Seqn(SetId('b, 5), 'b))))
          res
        }
      }

}
