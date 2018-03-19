object ex16 {
  def main(args: Array[String]): Unit = {
    def addOne(as: List[Int]): List[Int] = as match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

    println(addOne(List(1, 2, 3, 4)))
  }
}
