object ex10 {
  def main(args: Array[String]): Unit = {
    println(State.unit[Machine, Int](5).map2(State.unit[Machine, Int](10))(_ + _).run(Machine(false, 0, 0)))
    println(State.unit[Machine, Int](5).map2_0(State.unit[Machine, Int](10))(_ + _).run(Machine(false, 0, 0)))
    println(State.unit[Machine, Int](5).map2_2(State.unit[Machine, Int](10))(_ + _).run(Machine(false, 0, 0)))
  }
}
