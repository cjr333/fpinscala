object ex7 {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).map(x => x.toDouble).toList)
    println(Stream(1, 2, 3, 4).filter(x => (x % 2) == 0).toList)
    println(Stream(1, 2, 3, 4).append(Stream(5, 6)).toList)
    println(Stream(1, 2, 3, 4).flatMap(x => Stream(x.toDouble)).toList)

    println(Stream(1, 2, 3, 4).reverse.toList)
    println(Stream(1, 2, 3, 4).foldRight(0)((a, b) => a - b))
    println(Stream(1, 2, 3, 4).foldRightStackOverflow(0)((a, b) => a - b))
    println(Stream(1, 2, 3, 4).foldRight(Nil: List[Int])((a, b) => a :: b))
    println(Stream(1, 2, 3, 4).foldRightStackOverflow(Nil: List[Int])((a, b) => a :: b))
  }
}
