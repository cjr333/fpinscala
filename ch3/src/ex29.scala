import Tree._

object ex29 {
  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    println(size2(tree))
    println(maximum2(tree))
    println(depth2(tree))
    println(map2(tree)(_ + 1))
  }
}
