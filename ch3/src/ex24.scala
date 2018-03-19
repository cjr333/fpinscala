import List._

object ex24 {
  def main(args: Array[String]): Unit = {
    println(hasSubSequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubSequence(List(1, 2, 3, 4), Nil))
    println(hasSubSequence(List(1, 2, 3, 4), List(1, 4)))
    println(hasSubSequence(List(1, 2, 3, 4), List(3, 4)))
  }
}
