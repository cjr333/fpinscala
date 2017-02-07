
object ex8 {
  def main(args: Array[String]): Unit = {
    println(Stream.constant("1").take(5).toList)
    println(Stream.constant(1).take(5).toList)
    println(Stream.constant(1.0).take(5).toList)
  }
}
