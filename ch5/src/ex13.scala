object ex13 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).map2(x => x.toDouble).toList)
    println(Stream(1, 2, 3, 4).take(2).toList)
    println(Stream(1, 2, 3, 4, 5, 6).takeWhile3(x => x < 4).toList)

    println(Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList)
    println(Stream(1, 2, 3, 4).zipWith(Stream(4, 5, 6))(_ + _).toList)
    println(Stream(1, 2, 3).zipWith(Stream(4, 5, 6, 7))(_ + _).toList)

    println(Stream(1, 2, 3, 4).zipAll(Stream(3, 4)).toList)
    println(Stream(1, 2).zipAll(Stream(3, 4, 5, 6)).toList)
  }
}
