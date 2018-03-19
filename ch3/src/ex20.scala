import List._

object ex20 {
  def main(args: Array[String]): Unit = {
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
  }
}
