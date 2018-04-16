object ex9 {
  def main(args: Array[String]): Unit = {
    println(RNG.map(RNG.unit(5))(_ % 2)(SimpleRNG(1)))
    println(RNG.map_2(RNG.unit(5))(_ % 2)(SimpleRNG(1)))

    println(RNG.map2(RNG.unit(5), RNG.unit(10))(_ + _)(SimpleRNG(1)))
    println(RNG.map2_2(RNG.unit(5), RNG.unit(10))(_ + _)(SimpleRNG(1)))
  }
}
