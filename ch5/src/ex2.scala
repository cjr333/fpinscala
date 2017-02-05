object ex2 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).take(2).toList)
    println(Stream(1, 2, 3, 4).take(-1).toList)
    println(Stream.empty.take(2).toList)

    println(Stream(1, 2, 3, 4).drop(2).toList)
    println(Stream(1, 2, 3, 4).drop(-1).toList)
    println(Stream.empty.drop(2).toList)
  }
}
