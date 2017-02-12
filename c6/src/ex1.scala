object ex1 {
  def main(args: Array[String]): Unit = {
    println("Int.MaxValue : " + Int.MaxValue)
    println("Int.MinValue : " + Int.MinValue)

    println(SimpleRNG(11163).nextInt)
    println(RNG.nonNegativeInt(SimpleRNG(11163)))
  }
}
