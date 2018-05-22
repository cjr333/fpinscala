package ch7

object ex3 {
  def main(args: Array[String]): Unit = {
    val a = Par.unit(1)
    val b = Par.unit(2)
    Par.map2Advanced(a, b)(_ + _)

    val test: Option[Int] = Some(1)
    println(test)
  }
}
