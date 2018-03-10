object ex1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) b
      else go (b, a+b, n-1)
    }

    go(1, 0, n)
  }
  def main(args: Array[String]): Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
  }
}
