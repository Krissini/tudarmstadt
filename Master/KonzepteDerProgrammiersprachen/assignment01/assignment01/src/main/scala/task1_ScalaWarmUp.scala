object ScalaWarmUp {

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

 def flatten[A](xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }


  def reverse[A](l: List[A], result: List[A] = Nil): List[A] = l match {
    case Nil => result
    case (x :: xs) => reverse[A](xs, x :: result)
  }

}