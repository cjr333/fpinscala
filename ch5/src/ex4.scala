object ex4 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).exists(i => i < 3))
    println(Stream(1, 2, 3, 4).forAll(i => i < 3))
  }
}
