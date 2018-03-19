object ex17 {
  def main(args: Array[String]): Unit = {
    def doubleToString(ds: List[Double]): List[String] = ds match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

    println(doubleToString(List(1.0, 2.0, 3.0, 4.0)))
  }
}
