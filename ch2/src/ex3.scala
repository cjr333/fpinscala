object ex3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
  def main(args: Array[String]): Unit = {
    val curryF = curry[Int, Int, Int]((a, b) => a+b)
    println(curryF(1)(2))
  }
}
