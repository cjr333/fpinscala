case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => (n % 2) == 0))

  def listOf[A](g: Gen[A]): Gen[List[A]] = {
    val (n, rng) = RNG.nonNegativeInt(SimpleRNG(System.currentTimeMillis()))
    listOfN(n % 100, g)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
//    Gen(State.sequence(List.fill(n)(g.sample)))
    sequence(List.fill(n)(g))
  }

  def map2[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(ga.sample.map2(gb.sample)(f))
  }

  def sequence[A](list: List[Gen[A]]): Gen[List[A]] = {
    list.foldRight(unit(List[A]()))((ga, gl) => map2(ga, gl)(_ :: _))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val sum = g1._2 + g2._2
    Gen(State(RNG.double)).flatMap(d => if (d < g1._2 / sum) g1._1 else g2._1)
  }
}