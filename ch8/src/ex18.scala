import Gen._
import Prop._

object ex18 {
  def main(args: Array[String]): Unit = {
    val isEven = (i: Int) => i % 2 == 0
    val takenWhileProp = forAll(listOf(choose(0, 100)))(ns => ns.takeWhile(isEven).forall(isEven)) &&
      forAll(listOf(choose(0, 100)))(ns => ns.takeWhile(isEven).dropWhile(isEven).isEmpty)
    run(takenWhileProp)
  }
}
