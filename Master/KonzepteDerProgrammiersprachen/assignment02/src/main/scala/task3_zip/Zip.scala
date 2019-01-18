package task3_zip

object Zip {
  def zipop[A, B, C](f: (A, B) => C): (List[A], List[B]) => List[C] = {
    (l1: List[A], l2: List[B]) =>
      if (l1.isEmpty || l2.isEmpty)
        List.empty
      else
        f(l1.head, l2.head) :: zipop(f)(l1.tail, l2.tail)
  }

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

  def zipadd(lhs: List[Int], rhs: List[Int]): List[Int] = ???

  def zip[A, B](lhs: List[A], rhs: List[B]): List[(A, B)] = ???

  def addMatrix(lhs: List[List[Int]], rhs: List[List[Int]]): List[List[Int]] = ???
}
