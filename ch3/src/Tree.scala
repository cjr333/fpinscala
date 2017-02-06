/**
  * Created by cjr333 on 2017-01-30.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n:A) => f(n)
    case Branch(l:Tree[A], r:Tree[A]) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]) = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  def maximum2(t: Tree[Int]) = {
    fold(t)(n => n)(_ max _)
  }

  def depth2[A](t: Tree[A]) = {
    fold(t)(_ => 1)((x, y) => 1 + (x max y))
  }

  def map2[A, B](t: Tree[A])(f: A => B) = {
    fold(t)(n => Leaf(f(n)).asInstanceOf[Tree[B]])(Branch(_, _))
  }
}
