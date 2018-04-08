import Stream._

object ex8_9_10_11 {
  def main(args: Array[String]): Unit = {
    println(constant(1).take(5).toList)
    println(from(2).take(5).toList)
    println(fibs().take(5).toList)
    println(unfold(1)(n => Some((n, n + 1))).take(5).toList)
  }
}
