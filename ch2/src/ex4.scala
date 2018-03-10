object ex4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
  def main(args: Array[String]): Unit = {
    val uncurryF = uncurry[Int, Int, Int](a => b => a + b)
    println(uncurryF(1, 2))
  }
}
