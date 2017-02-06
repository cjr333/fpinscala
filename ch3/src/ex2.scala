object ex2 {
  def main(args: Array[String]): Unit = {
    println(List.tail(Nil))
    println(List.tail(List(1, 2, 3, 4)))
    println(List.tail(List("a", "b", "c", "d")))
    println(List.tail(List(1.0, 2.0, 3.0, 4.0)))
  }
}
