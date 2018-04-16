
object ex11 {
  def main(args: Array[String]): Unit = {
    val s1 = simulationMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))
    println(s1)
    val s2 = simulationMachine(List(Coin, Turn, Turn, Coin)).run(s1._2)
    println(s2)
    val s3 = simulationMachine(List(Charge, Turn, Charge, Turn)).run(s2._2)
    println(s3)
    val s4 = simulationMachine(List(Coin, Turn, Coin, Coin, Coin, Coin, Turn)).run(s3._2)
    println(s4)
  }

  def simulationMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State((s:Machine) => {
      val machine = inputs.foldLeft(s)((machine, input) => Machine.handleInput(machine, input))
      ((machine.candies, machine.coins), machine)
    })
  }
}
