/**
  * Created by cjr333 on 2017-01-30.
  */
object ex29 {
  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(3), Leaf(2)))
    println(Tree.size(t))
    println(Tree.maximum(t))
    println(Tree.depth(t))
    println(Tree.map(t)(_.toDouble))

    println(Tree.size2(t))
    println(Tree.maximum2(t))
    println(Tree.depth2(t))
    println(Tree.map2(t)(_.toDouble))
  }
}
