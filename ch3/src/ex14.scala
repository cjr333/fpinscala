import List._

object ex14 {
  def main(args: Array[String]): Unit = {
    def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] =
      foldRightUsingFoldLeft(a1, a2)(Cons(_, _))

    println(appendUsingFoldRight(List(1, 2, 3), List(4, 5, 6)))
  }
}
