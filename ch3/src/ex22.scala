import List._

object ex22 {
  def main(args: Array[String]): Unit = {
    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
    println(zipWith(List(1, 2, 3), List(4, 5, 6, 7))(_ + _))
    println(zipWith(List(1, 2, 3, 4), List(4, 5, 6))(_ + _))
  }
}
