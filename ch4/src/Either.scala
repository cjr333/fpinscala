sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >:E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def map2_2[EE >:E, B, C](b: Either[EE, B])(f: (A, B) => C)(g: (EE, EE) => EE): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(a), Left(ee)) => Left(ee)
    case (Left(e), Right(b)) => Left(e)
    case (Left(e), Left(ee)) => Left(g(e, ee))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]