import List._

object ex8 {
  def main(args: Array[String]): Unit = {
    // List(1, 2, 3)을 다시 리턴하겠지..
    println(foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
  }
}
