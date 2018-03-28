import Option.sequence;

object ex4 {
  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))
  }
}
