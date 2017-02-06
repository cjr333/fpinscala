/**
  * Created by cjr333 on 2017-01-30.
  */
object ex10 {
  def main(args: Array[String]): Unit = {
    // init list
    val x = List.makeInts(Nil, 1000000)

    // stackoverflow
    //List.sum2(x)

    // stack-safe
    println(List.sum3(x))
  }
}
