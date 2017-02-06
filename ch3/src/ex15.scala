/**
  * Created by cjr333 on 2017-01-30.
  */
object ex15 {
  def main(args: Array[String]): Unit = {
    val x = List(List(1, 2, 3), List(4, 5), List(6))
    println(x)
    println(List.flatMap(x))
  }
}
