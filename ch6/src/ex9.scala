object ex9 {
  def main(args: Array[String]): Unit = {
    println(RNG.map(RNG.unit(5))(_ % 2))
    println(RNG.map1_2(RNG.unit(5))(_ % 2))

    println(RNG.map2(RNG.unit(5), RNG.unit(10))(_ + _))
    println(RNG.map2_2(RNG.unit(5), RNG.unit(10))(_ + _))
  }
}
