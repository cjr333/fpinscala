import List._

object ex3 {
  def main(args: Array[String]): Unit = {
    println(setHead(Nil, 1))
    println(setHead(List(1, 2, 3, 4), 5))
  }
}
