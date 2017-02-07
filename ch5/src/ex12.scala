
object ex12 {
  def main(args: Array[String]): Unit = {
    println(Stream.ones2.take(5).toList)
    println(Stream.constant2(1.0).take(5).toList)
    println(Stream.from2(5).take(5).toList)
    println(Stream.fibs2.take(10).toList)
  }
}
