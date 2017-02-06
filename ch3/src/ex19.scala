/**
  * Created by cjr333 on 2017-01-30.
  */
object ex19 {
  def main(args: Array[String]): Unit = {
    println(List.filter(Nil: List[Int])(x => x % 2 == 0))
    println(List.filter(List(1, 2, 3, 4))(x => x % 2 == 0))
  }
}
