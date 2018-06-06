import Gen._
import Prop._

object ex13 {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf(smallInt)) { ns =>
//      println(ns.size)
      if (ns.size > 0) {
        val max = ns.max
        !ns.exists(_ > max)
      } else {
        true
      }
    }
    run(maxProp)
  }
}
