object ex5 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 3, 5, 2, 4, 6).takeWhile(_ < 4).toList)
  }
}
