import List._

object ex5 {
  def main(args: Array[String]): Unit = {
    println(dropWhile(Nil, (_: Int) => true))
    println(dropWhile(List(1, 2, 3, 4), (_: Int) => true))
    println(dropWhile(List(1, 2, 3, 4), (i: Int) => i < 2))
  }
}
