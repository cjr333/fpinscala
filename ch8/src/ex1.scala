import Prop._

object ex1 {
  def main(args: Array[String]): Unit = {
    val intList = Gen.listOf(Gen.choose(0, 100))

    val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
    forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    val failingProp = forAll(intList)(ns => ns.reverse == ns)

    val simpleRNG = SimpleRNG(System.currentTimeMillis())
    println(prop.run(10, simpleRNG))
    println(failingProp.run(10, simpleRNG))
  }
}
