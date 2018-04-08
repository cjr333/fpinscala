object ex14 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    println(Stream(1, 2, 3).startsWith(Stream(1, 2, 3)))
    println(Stream(1, 2, 3).startsWith(Stream(2, 3)))
    println(Stream(1, 2, 3).startsWith(Stream(1, 3)))
  }
}
