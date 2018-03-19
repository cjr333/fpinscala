
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], nh: A): List[A] = l match {
    case Nil => Cons(nh, Nil)
    case Cons(_, t) => Cons(nh, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, b) => b + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def flatten[A](ass: List[List[A]]): List[A] = {
    foldRightUsingFoldLeft(ass, Nil: List[A])(append)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) if !f(h) => filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = as match {
    case Nil => Nil
    case Cons(ah, at) => bs match {
      case Nil => Nil
      case Cons(bh, bt) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(ph, pt), Cons(bh, bt)) if ph == bh => startsWith(pt, bt)
    case _ => false
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(_, _), Cons(_, _)) if startsWith(sup, sub) => true
    case (Cons(_, pt), Cons(_, _)) if !startsWith(sup, sub) => hasSubSequence(pt, sub)
  }
}
