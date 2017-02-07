import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toListStackOverflow: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toListStackOverflow
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(reverse: List[A], remainder: Stream[A]): List[A] = {
      remainder match {
        case Empty => reverse
        case Cons(h, t) => go(h() :: reverse, t())
      }
    }

    go(List(), this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case Cons(_, _) if n == 0 => empty
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ if n == 0 => this
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = {
    println("forAll is called")
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
  }

  def foldRightStackOverflow[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this.reverse.foldLeft(z)((b, a) => f(a, b))
  }

  def reverse: Stream[A] = {
    foldLeft(empty[A])((a, b) => cons(b, a))
  }

  @annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty )
  }

  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => p(a) match {
      case true => cons(a, b)
      case false => b
    })
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }

  def map3[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take3(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhile3(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (Some(a), None) => true
      case _ => false
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    }.append(Stream(empty))
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
    unfold((this.reverse, z)) {
      case (Cons(h, t), z) => Some((f(h(), z), (t(), f(h(), z))))
      case _ => None
    }.reverse.append(Stream(z))
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

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  def ones2: Stream[Int] = {
    unfold(1)(_ => Some((1,1)))
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(n => Some((n, n + 1)))
  }

  def fibs2: Stream[Int] = {
    unfold((0, 1)){ case (a, b) => Some((a, (b, a + b)))}
  }
}
