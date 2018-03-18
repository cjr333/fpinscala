import List._

object ex11 {
  def main(args: Array[String]): Unit = {
    def sum(ints: List[Int]) =
      foldLeft(ints, 0)(_ + _)

    def product(ds: List[Double]) =
      foldLeft(ds, 1.0)(_ * _)

    def length[A](l: List[A]): Int =
      foldLeft(l, 0)((b, _) => b + 1)

    println(sum(List(1, 2, 3)))
    println(product(List(1, 2, 3)))
    println(length(List(1, 2, 3)))
  }
}
