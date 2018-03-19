import List._

object ex21 {
  def main(args: Array[String]): Unit = {
    def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if (f(a)) List(a) else Nil)
    }

    println(filterUsingFlatMap(List(1, 2, 3, 4))(x => (x % 2) == 0))
  }
}
