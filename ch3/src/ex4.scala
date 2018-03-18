import List._

object ex4 {
  def main(args: Array[String]): Unit = {
    println(drop(Nil, 1))
    println(drop(List(1, 2, 3, 4), 2))
    println(drop(List(1, 2, 3, 4), 5))
  }
}
