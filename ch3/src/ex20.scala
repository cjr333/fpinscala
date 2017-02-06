/**
  * Created by cjr333 on 2017-01-30.
  */
object ex20 {
  def main(args: Array[String]): Unit = {
    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
  }
}
