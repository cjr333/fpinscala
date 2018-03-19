import List._

object ex23 {
  def main(args: Array[String]): Unit = {
    println(zipWith(List(1, 2, 3), List(1.0, 2.0, 3.0))(_ * _))
    println(zipWith(List("1", "2", "3"), List("1.0", "2.0", "3.0"))(_ + _))
  }
}
