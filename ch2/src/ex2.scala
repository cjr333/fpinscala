object ex2 {
  def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A], ordered: (A,A) => Boolean, n: Int): Boolean = {
      if (n >=as.length - 1) true
      else if (ordered(as(n), as(n+1))) go(as, ordered, n+1)
      else false
    }
    go(as, ordered, 0)
  }

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else if (ordered(as(0), as(1))) isSorted(as.drop(1), ordered)
    else false
  }

  def main(args: Array[String]): Unit = {
    println(isSorted[Int](Array(1, 2, 3, 4, 5), (x, y) => x <= y))
    println(isSorted(Array(1, 2, 4, 3, 5), (x: Int, y: Int) => x <= y))
    println(isSorted(Array("1", "2", "3", "4", "5"), (x: String, y: String) => x <= y))
    println(isSorted(Array("1", "2", "4", "3", "5"), (x: String, y: String) => x <= y))

    println(isSorted2(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x <= y))
    println(isSorted2(Array(1, 2, 4, 3, 5), (x: Int, y: Int) => x <= y))
    println(isSorted2(Array("1", "2", "3", "4", "5"), (x: String, y: String) => x <= y))
    println(isSorted2(Array("1", "2", "4", "3", "5"), (x: String, y: String) => x <= y))
  }
}
