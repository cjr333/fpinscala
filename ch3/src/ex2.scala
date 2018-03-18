import List._

object ex2 {
  def main(args: Array[String]): Unit = {
    println(tail(Nil))
    println(tail(List(1, 2, 3, 4)))
  }
}
