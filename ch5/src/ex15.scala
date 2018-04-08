object ex15 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).tails.toList.length)
    Stream(1, 2, 3).tails.toList.foreach(s => println(s.toList))

    println(Stream(1, 2, 3, 4).hasSubsequence(Stream(1, 2)))
    println(Stream(1, 2, 3, 4).hasSubsequence(Stream(3, 4)))
    println(Stream(1, 2, 3, 4).hasSubsequence(Stream(1, 4)))
  }
}
