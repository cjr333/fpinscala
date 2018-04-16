object ex8 {
  def main(args: Array[String]): Unit = {
    println(RNG.nonNegativeInt(BeginWithBigInt(123456)))
    println(RNG.nonNegativeLessThan(Int.MaxValue - 123456)(BeginWithBigInt(123456)))
  }
}
