import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toListStackOverflow: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListStackOverflow
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(reverse: List[A], remainder: Stream[A]): List[A] = remainder match {
      case Empty => reverse
      case Cons(h, t) => go(h() :: reverse, t())
    }
    go(Nil, this).reverse
  }

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(reverse: Stream[A], remainder: Stream[A], n: Int): Stream[A] = remainder match {
      case Empty => reverse
      case Cons(h, t) if n > 0 => go(cons(h(), reverse), t(), n - 1)
      case _ if n == 0 => reverse
      case _ => empty
    }
    go(Empty, this, n).reverse
  }

  def reverse: Stream[A] = {
    @annotation.tailrec
    def go(reverse: Stream[A], remainder: Stream[A]): Stream[A] = remainder match {
      case Empty => reverse
      case Cons(h, t) => go(cons(h(), reverse), t())
    }
    go(Empty, this)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n- 1)
    case _ if n == 0 => this
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def go(reverse: Stream[A], remainder: Stream[A]): Stream[A] = remainder match {
      case Empty => reverse
      case Cons(h, t) if p(h()) => go(cons(h(), reverse), t())
      case Cons(h, t) if !p(h()) => reverse
      case _ => empty
    }
    go(Empty, this).reverse
  }

  def foldRightStackOverflow[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRightStackOverflow(z)(f))
    case _ => z
  }

  @annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    case _ => z
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this.reverse.foldLeft(z)((b, a) => f(a, b))
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => {
      println(a)
      p(a) || b
    })
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => {
      println(a)
      p(a) && b
    })
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, as) => {
      println(a)
      if (p(a)) {
        cons(a, as)
      } else {
        empty
      }
    })
  }

  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, bs) => cons(f(a), bs))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, as) => if(f(a)) cons(a, as) else as)
  }

  def append[B >: A](as: Stream[B]): Stream[B] = {
    foldRight(as)((a, as2) => cons(a, as2))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, bs) => f(a).append(bs))
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] =  {
    unfold((this, n)){
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, bs){
      case ((Cons(ah, at), Cons(bh, bt))) => Some(f(ah(), bh()), (at(), bt()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this, s2){
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Cons(ah, at), _) => Some((Some(ah()), None), (at(), empty[B]))
      case (_, Cons(bh, bt)) => Some((None, Some(bh())), (empty[A], bt()))
      case _ => None
    }
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case(Some(a), _) => true
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    }.append(Stream(empty))
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails.exists(_ startsWith(s))
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this match {
    case Cons(h, t) => {
      lazy val subStream = t().scanRight(z)(f)
      Stream(f(h(), subStream.headOption.get)).append(subStream)
    }
    case _ => Stream(z)
  }

  def scanRight2[B](z: B)(f: (A, B) => B): Stream[B] = {
    unfold(this.reverse, z) {
      case (Cons(h, t), z) => Some(f(h(), z), (t(), f(h(), z)))
      case _ => None
    }.reverse.append(Stream(z))
  }

  def scanRight3[B](z: B)(f: (A, B) => B): Stream[B] = {
    unfold(this.reverse, z) {
      case (Cons(h, t), z) => {
        val added = f(h(), z)
        Some(added, (t(), added))
      }
      case _ => None
    }.reverse.append(Stream(z))
  }

  def scanRight4[B](z: B)(f: (A, B) => B): Stream[B] = {
    foldRight(Stream(z))((a, bs) => Stream(f(a, bs.headOption.get)).append(bs))
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def ones2: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n + 1))
  }

  def fibs2(): Stream[Int] = {
    unfold((0, 1)){case (a, b) => Some(a, (b, a + b))}
  }
}
