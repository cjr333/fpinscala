object ex5 {
  def main(args: Array[String]): Unit = {
    println(parseInts(List("1", "2", "3")))
    println(parseInts(List("1", "a", "3")))
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))
  }

  def Try[A, B](a: => A)(f: A => B): Option[B] = {
    try Some(f(a))
    catch { case e: Exception => None }
  }

  def parseInts(a: List[String]) : Option[List[Int]] = {
    traverse(a)(a => Try(a)(_.toInt))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap(bb => traverse(t)(f) map (bb :: _))
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
}
