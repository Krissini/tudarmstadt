object Task1 {
  sealed trait PeanoNumber
  case object Zero extends PeanoNumber
  case class Succ(pred: PeanoNumber) extends PeanoNumber //constructor

  // pattern matching example
  def pred(x: PeanoNumber): PeanoNumber = x match { // takes peano# und outputted piano#
    case Zero => Zero
    case Succ(p) => p // succ(succ(.......(zero)))
  }
  def succ(x: PeanoNumber): PeanoNumber = Succ(x) // constructors

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  //your solution here, assume that the arguments are only natural numbers
  def add(n: PeanoNumber, m: PeanoNumber): PeanoNumber = n match {
    case Zero => m
    case Succ(p) => succ(add(p, m))
  }

}
