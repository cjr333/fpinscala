object ex7 {
  def main(args: Array[String]): Unit = {
    println(RNG.ints(5)(SimpleRNG(5)))
    println(RNG.ints2(5)(SimpleRNG(5)))
  }
}
