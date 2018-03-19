import Tree._

object ex26 {
  def main(args: Array[String]): Unit = {
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    println(maximum(Leaf(1)))
    println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))
    println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
  }
}
