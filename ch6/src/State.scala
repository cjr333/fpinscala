import State._

case class State[S, +A](run: S => (A, S)) {
  def map_0[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })
  }

  def map2_0[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
  }

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def map2_2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- this
      b <- sb
    } yield f(a, b)
  }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S, A](sl: List[State[S, A]]): State[S, List[A]] = {
    sl.foldRight(unit[S, List[A]](List()))((state, listState) => state.map2(listState)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}