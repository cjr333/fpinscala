object ex2 {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(11)
    println(RNG.double(rng))

    val kk2: List[(Int, Double)] = (1, 0.5) :: List((2, 0.6))
    println(kk2)
    println(kk2.map{ case (x, y) => x })
    println(kk2.map(_._1))
    val kk: List[Int] = List(2, 3)
    println(kk.map(x => x + 1))
  }
}
