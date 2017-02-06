/**
  * Created by cjr333 on 2017-01-30.
  */
object ex17 {
  def main(args: Array[String]): Unit = {
    val x = List(1.0, 2.0, 3.0)
    apply(x)(d => println(d.getClass))
    apply(doubleToString(x))(s => println(s.getClass))
  }

  def doubleToString(ds: List[Double]): List[String] = {
    List.foldRight(ds, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  def apply[A](as: List[A])(f: A => Unit): Unit = as match {
    case Nil => Unit
    case Cons(x, xs) => {
      f(x)
      apply(xs)(f)
    }
  }
}
