object ex4 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).forAll(x => x < 2))
  }
}
