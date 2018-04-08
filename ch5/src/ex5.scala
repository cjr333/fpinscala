object ex5 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile2(x => x < 4).toList)
  }
}
