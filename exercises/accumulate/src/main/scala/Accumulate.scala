class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = list match {
    case x :: xs => f(x) :: accumulate(f, xs)
    case Nil => Nil
  }
}
