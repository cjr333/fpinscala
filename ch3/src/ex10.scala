import List._

object ex10 {
  def main(args: Array[String]): Unit = {
    // setup data
    @annotation.tailrec
    def loop(l: List[Int], n: Int): List[Int] = n match {
      case 0 => l
      case x if x > 0 => loop(Cons(n, l), n - 1)
      case _ => Nil
    }

    val data = loop(Nil, 10000)
    try {
      println(foldRight(data, 0)(_ + _))
    } catch {
      case e: Throwable => println(e)
    }

    println(foldLeft(data, 0)(_ + _))
  }
}
