import Tree._

object ex27 {
  def main(args: Array[String]): Unit = {
    println(depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))))
    println(depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
  }
}
