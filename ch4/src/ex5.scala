import Option.sequence2

object ex5 {
  def main(args: Array[String]): Unit = {
    println(sequence2(List(Some(1), Some(2), Some(3))))
    println(sequence2(List(Some(1), None, Some(3))))
  }
}
