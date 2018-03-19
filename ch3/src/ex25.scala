import Tree._

object ex25 {
  def main(args: Array[String]): Unit = {
    println(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
  }
}
