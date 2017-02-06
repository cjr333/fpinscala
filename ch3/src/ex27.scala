/**
  * Created by cjr333 on 2017-01-30.
  */
object ex27 {
  def main(args: Array[String]): Unit = {
    val a = Branch(Branch(Branch(Leaf("a"), Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3")))), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    println(Tree.depth(a))
  }
}
