import Gen._
import Prop._

object ex20 {
  def main(args: Array[String]): Unit = {
    val isEven = (i: Int) => i % 2 == 0
    val ints = listOf(choose(0, 100))
    val takeProp = forAll(ints)(ns => ns.take(0).isEmpty) &&
      forAll(ints)(ns => (ns ++ List(1, 2, 3)).take(ns.size).equals(ns))
    run(takeProp)
    val dropProp = forAll(ints)(ns => ns.drop(0).equals(ns)) &&
      forAll(ints)(ns => (List(1, 2, 3) ++ ns).drop(3).equals(ns)) &&
      forAll(ints)(ns => ns.drop(ns.size).isEmpty)
    run(dropProp)
    val filterProp = forAll(ints)(ns => ns.filter(i => false).isEmpty) &&
      forAll(ints)(ns => ns.filter(i => true).equals(ns)) &&
      forAll(ints)(ns => ns.filter(isEven).forall(isEven))
    val unfoldProp = forAll(choose(0, 100))(n => Stream.constant2(n).take(10).toList.size == 10)
    run(unfoldProp)

    val foldProp = forAll(treeOfD(5, choose(0, 100)))(tree => Tree.fold(tree)(_ >= 0)(_ && _))
    run(foldProp)

    val someInts = listOf(choose(0, 100).map(Some(_)))
    val sequneceProp = forAll(someInts)(os => Option.sequence(os).getOrElse(null).size == os.size) &&
      forAll(someInts)(os => Option.sequence(os ++ List(None)) == None)
    run(sequneceProp)
  }
}
