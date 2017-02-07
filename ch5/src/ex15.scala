
object ex15 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).tails.toList)
    println(Stream(1, 2, 3).tails.foldLeft("")((a, b) => a + "," + b.toList.toString()))
  }
}
