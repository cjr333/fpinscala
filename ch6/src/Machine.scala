sealed trait Input

case object Coin extends Input

case object Turn extends Input

case object Charge extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def handleInput(machine: Machine, input: Input): Machine = (machine, input) match {
    case (Machine(locked, candies, coins), Charge) => Machine(locked, candies + 10, coins)
    case (Machine(true, candies, coins), Coin) if candies > 0 => Machine(false, candies, coins + 1)
    case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
    case _ => machine
//    case (Machine(true, _, _), Turn) => machine
//    case (Machine(false, _, _), Coin) => machine
//    case _ => machine
  }
}