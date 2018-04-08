object ex16 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).scanRight(0)(add).toList)
    println(Stream(1, 2, 3).scanRight2(0)(add).toList)
    println(Stream(1, 2, 3).scanRight3(0)(add).toList)
  }

  def add(a: Int, b: Int): Int = {
    println(a + b)
    a + b
  }
}
