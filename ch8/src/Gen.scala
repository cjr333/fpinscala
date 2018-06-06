case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def map[B](f: A => B): Gen[B] = {
    flatMap(a => Gen.unit(f(a)))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def stream: Gen[Stream[A]] = {
    Gen(State(rng => {
      (Stream.unfold(rng)(rng => scala.Some(this.sample.run(rng))), rng)
    }))
  }

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = Gen.map2(this, g)((_, _))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => (n % 2) == 0))

//  def listOf[A](g: Gen[A]): Gen[List[A]] = {
//    val (n, rng) = RNG.nonNegativeInt(SimpleRNG(System.currentTimeMillis()))
//    listOfN(n % 100, g)
//  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(size => listOfN(size, g))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
//    Gen(State.sequence(List.fill(n)(g.sample)))
    sequence(List.fill(n)(g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(_ => listOfN(0, g))
  }

  def treeOfD[A](d: Int, g: Gen[A]): Gen[Tree[A]] = {
    def go(d: Int, g: Gen[A], rng: RNG): (Tree[A], RNG) = {
      if (d > 0) {
        val (left, rng2) = go(d - 1, g, rng)
        val (right, rng3) = go(d - 1, g, rng2)
        (Branch(left, right), rng3)
      } else {
        val (a, rng2) = g.sample.run(rng)
        (Leaf(a), rng2)
      }
    }
    Gen(State(rng => go (d, g, rng)))
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

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(size => this.forSize(size).flatMap(f(_).forSize(size)))
}