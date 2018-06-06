import Gen._
import Prop._
import ch7.Par

object ex16 {
  def main(args: Array[String]): Unit = {
    val pint = Gen.choose(0, 10).map(Par.unit(_))
    val p4 = forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
    run(p4)
    val pint2: Gen[Par.Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
      l.foldLeft(Par.unit(0))((p,i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

    val forkProp = forAllPar(pint2)(n => Par.equal(Par.fork(n), n))
    run(p4)
  }
}
