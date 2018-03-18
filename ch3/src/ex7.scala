import List._

object ex7 {
  // 즉시 재귀를 멈추지 않음.. 그래서 StackOverflow 가 발생할 것을 확인
  def main(args: Array[String]): Unit = {
    // setup data
    @annotation.tailrec
    def loop(l: List[Double], n: Int): List[Double] = n match {
      case 0 => l
      case x if x > 0 => loop(Cons(n, l), n - 1)
      case _ => Nil
    }

    val data = setHead(loop(Nil, 10000), 0.0)
    println(head(data))
    println(product(data))
    println(product2(data))
  }
}
