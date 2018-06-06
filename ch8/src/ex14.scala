import Gen._
import Prop._

object ex14 {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = forAll(listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      isSorted(sorted)
    }
    run(sortedProp)

    val alphabets = "abcdefghijklmnopqrstuvwxyz"
    val smallString = Gen.choose(0, 26).flatMap(i => Gen.unit(alphabets.substring(i)))
    val sortedProp2 = forAll(listOf(smallString)) { ns =>
      isSorted(ns.sorted)((s1, s2) => s1.compare(s2))
    }
    run(sortedProp2)
  }

  def isSorted[A](list: List[A])(implicit ord: Ordering[A]): Boolean = list match {
    case Nil => true
    case _ :: Nil => true
    case a :: as => ord.lteq(a, as.head) && isSorted(as)(ord)
  }
}
