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
    case Cons(_, t) => t().takeWhile(p)
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
    foldRight(empty[A])((a, b) => cons(a, b))
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
}
