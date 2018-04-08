object ex3 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile(x => x < 5).toList)
  }
}
