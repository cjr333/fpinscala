
object twoIntSum {
  def main(args: Array[String]): Unit = {
//    val a = scala.io.StdIn.readInt();
//    val b = scala.io.StdIn.readInt();
//    println(a + b)

    println(scala.io.Source.stdin.getLines().take(2).map(_.toInt).sum)
  }
}
