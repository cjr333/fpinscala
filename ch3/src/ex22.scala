/**
  * Created by cjr333 on 2017-01-30.
  */
object ex22 {
  def main(args: Array[String]): Unit = {
    println(addTwoList(List(1, 2, 3), List(4, 5, 6)))
    println(productTwoList(List(1.0, 2.0, 3.0), List(4, 5, 6)))
  }

  def addTwoList(a1: List[Int], a2: List[Int]): List[Int] = {
    List.zipWith(a1, a2)(_ + _)
  }

  def productTwoList(a1: List[Double], a2: List[Double]): List[Double] = {
    List.zipWith(a1, a2)(_ * _)
  }
}
