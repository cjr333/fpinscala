trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (Math.abs(i + 1), rng2)
    else (i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(list: List[Int], rng: RNG, count: Int): (List[Int], RNG) = {
      if (count > 0) {
        val (i, rng2) = rng.nextInt
        go(i :: list, rng2, count - 1)
      } else {
        (list, rng)
      }
    }

    go(List(), rng, count)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val int: Rand[Int] = _.nextInt

  def randIntDouble: Rand[(Int, Double)] = {
    both(int, double)
  }

  def randDoubleInt: Rand[(Double, Int)] = {
    both(double, int)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List[A](), rng))((rand, tuple) => {
        val (a, rng2) = rand(tuple._2)
        (a :: tuple._1, rng2)
      })
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((rand, listRand) => map2(rand, listRand)(_ :: _))
  }

  def ints2[A](count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence2(List.fill(count)(int))(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    println("nonNegativeLessThan")
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })
  }

  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class BeginWithBigInt(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = Int.MaxValue - seed;
    val nextRNG = BeginWithBigInt(newSeed)
    val n = (newSeed).toInt
    (n, nextRNG)
  }
}