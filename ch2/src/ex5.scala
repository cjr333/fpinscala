object ex5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
  def main(args: Array[String]): Unit = {
    println(compose[Int, Int, String](b => b.toString + b.toString, a => a*2)(2))
  }
}
