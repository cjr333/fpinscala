import com.sun.org.apache.xpath.internal.functions.FuncStartsWith

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => {
      //println(x)
      x * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](lists: List[A]): List[A] = lists match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](head: A, lists: List[A]): List[A] = lists match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => n match {
      case 0 => l
      case _ => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => dropWhile(xs, f)
      case false => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => {
      //println(x)
      f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  def makeInts(as: List[Int], n: Int): List[Int] = n match {
    case 0 => as
    case _ => makeInts(Cons(n, as), n - 1)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Double]) = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length3[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, y) => 1 + x)
  }

  def inverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def flatMap[A](ass: List[List[A]]): List[A] = {
    foldRight(ass, Nil: List[A])(append(_, _))
  }

  def addOne(ns: List[Int]): List[Int] = {
    foldRight(ns, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, y) => f(x) match {
      case true => Cons(x, y)
      case false => y
    })
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatMap(map(as)(f))
  }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    println("hasSubSequence")
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, Cons(x, xs)) => false
      case (Cons(x, xs), _) => startsWith(sup, sub) || hasSubSequence(xs, sub)
    }
  }

  @tailrec
  def hasSubSequence2[A](sup: List[A], sub: List[A]): Boolean = {
    println("hasSubSequence2")
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, Cons(x, xs)) => false
      case (Cons(x, xs), _) if startsWith(sup, sub) => true
      case (Cons(x, xs), _) => hasSubSequence2(xs, sub)
    }
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, Nil) => true
      case (Cons(x, xs), Nil) => true
      case (Nil, Cons(x, xs)) => false
      case (Cons(x, xs), Cons(y, ys)) => (x == y) && startsWith(xs, ys)
    }
  }
}