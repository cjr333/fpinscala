/**
  * Created by cjr333 on 2017-01-30.
  */
object ex8 {
  def main(args: Array[String]): Unit = {
    println(List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)))
  }
}
