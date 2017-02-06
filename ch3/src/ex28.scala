/**
  * Created by cjr333 on 2017-01-30.
  */
object ex28 {
  def main(args: Array[String]): Unit = {
    val a = Branch(Branch(Leaf(1), Leaf(6)), Branch(Leaf(5), Leaf(3)))
    println(Tree.map(a)(a => a.toDouble))
  }
}