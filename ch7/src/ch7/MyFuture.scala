package ch7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

sealed trait MyFuture[A] {
  private[ch7] def apply(cb: A => Unit): Unit
}

object MyPar {
  type MyPar[A] = ExecutorService => MyFuture[A]

  def run[A](es: ExecutorService)(p: MyPar[A]): Either[Exception, A] = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)
    try {
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      Right(ref.get())
    } catch { case e: Exception => Left(e)}
  }

  def unit[A](a: A): MyPar[A] = {
    es => new MyFuture[A] {
      override private[ch7] def apply(cb: (A) => Unit) =
        cb(a)
    }
  }

  def fork[A](a: => MyPar[A]): MyPar[A] = {
    es => {
//      println(Thread.currentThread().getId())
      new MyFuture[A] {
        override private[ch7] def apply(cb: (A) => Unit) = {
          eval(es){
            println(Thread.currentThread().getId())
            (a(es)(cb))
          }
        }
      }
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def map2[A, B, C](p: MyPar[A], p2: MyPar[B])(f: (A, B) => C): MyPar[C] = {
    es => new MyFuture[C] {
      override private[ch7] def apply(cb: (C) => Unit) = {
        var ar: Option[A] = None
        var br: Option[B] = None

        var combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }
  }
}
