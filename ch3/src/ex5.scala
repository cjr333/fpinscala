/**
  * Created by cjr333 on 2017-01-30.
  */
object ex5 {
  def main(args: Array[String]): Unit = {
    println(List.dropWhile(List(1, 2, 3, 4, 5, 6, 7), (x: Int) => x < 4))
    println(List.dropWhile(List(1, 2, 3, 4, 5, 6, 7), (x: Int) => x % 2 == 1))
  }
}
