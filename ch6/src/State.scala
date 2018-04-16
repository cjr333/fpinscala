
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](List()))((s, slist) => s.map2(slist)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}