import Stream._

object ex12 {
  def main(args: Array[String]): Unit = {
    println(ones2.take(5).toList)
    println(constant2(1).take(5).toList)
    println(from2(2).take(5).toList)
    println(fibs2().take(5).toList)
  }
}
