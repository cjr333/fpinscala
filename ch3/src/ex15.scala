import List._

object ex15 {
  def main(args: Array[String]): Unit = {
    // BigO(n - 모든 요소 수) 에 처리되도록 주의하자.
    println(flatten(List(List(1, 2), List(3, 4), List(5, 6))))
  }
}
