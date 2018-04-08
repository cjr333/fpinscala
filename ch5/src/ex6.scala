object ex6 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).headOption2)
    println(Stream().headOption2)
    println(Stream.empty.headOption2)
  }
}
