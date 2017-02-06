/**
  * Created by cjr333 on 2017-01-30.
  */
object ex7 {
  def main(args: Array[String]): Unit = {
    println("Product1 ============")
    List.product(List(1, 2, 3, 0, 4, 5))
    println("Product2 ============")
    List.product2(List(1, 2, 3, 0, 4, 5))
  }
}
