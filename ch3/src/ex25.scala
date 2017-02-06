/**
  * Created by cjr333 on 2017-01-30.
  */
object ex25 {
  def main(args: Array[String]): Unit = {
    println(Tree.size(Branch(Leaf("a"), Leaf("b"))))
    val a = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    println(Tree.size(a))
  }
}
