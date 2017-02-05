object ex6 {
  def main(args: Array[String]): Unit = {
    println(Stream.empty.headOption)
    println(Stream(1, 2).headOption)

    println(Stream.empty.headOption2)
    println(Stream(1, 2).headOption2)
  }
}
