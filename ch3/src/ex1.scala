import List._

object ex1 {
  def main(args: Array[String]): Unit = {
    val ret = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y   // print 3
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(ret)
  }
}
