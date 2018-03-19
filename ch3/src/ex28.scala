import Tree._

object ex28 {
  def main(args: Array[String]): Unit = {
    println(map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1))
  }
}
