/**
  * Created by cjr333 on 2017-01-30.
  */
object ex21 {
  def main(args: Array[String]): Unit = {
    println(filter(List(1, 2, 3, 4))(x => x % 2 == 0))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    List.flatMap(as)(x => f(x) match {
      case true => List(x)
      case false => Nil
    })
  }
}
