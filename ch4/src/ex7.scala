object ex7 {
  def main(args: Array[String]): Unit = {

  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((x, y) => x.map2(y)(_ :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))
  }
}
