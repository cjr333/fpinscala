object ex5 {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(11)

    println(RNG.double(rng))
    println(RNG.double2(rng))
  }
}
