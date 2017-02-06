object ex4 {
  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =  {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)((a, list) => a :: list))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def parseInts(a: List[String]) : Option[List[Int]] = {
    sequence(a map (i => Try(i.toInt)))
  }
}
