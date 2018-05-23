package ch7

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, unit: TimeUnit) = get
    override def isCancelled = false
    override def cancel(mayInterruptIfRunning: Boolean) = false
  }

  def unit[A](a: => A): Par[A] = {
    _ => UnitFuture(a)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }
  def fork[A](a: => Par[A]): Par[A] = {
    es => {
      println(Thread.currentThread().getId())
      es.submit(new Callable[A] {
        def call = {
          println(Thread.currentThread().getId())
          a(es).get
        }
      })
    }
  }

  def map2Advanced[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es => Map2Future(a(es), b(es))(f)
  }

  private case class Map2Future[A, B, C](af: Future[A], bf: Future[B])(f: (A, B) => C) extends Future[C] {
    override def isDone = af.isDone && bf.isDone
    override def get() = f(af.get(), bf.get())
    override def get(timeout: Long, unit: TimeUnit) = {
      val start = System.nanoTime()
      val aget = af.get(timeout, unit)
      val end = System.nanoTime()
      val remainNano = TimeUnit.NANOSECONDS.convert(timeout, unit) - (end - start)
      val bget = bf.get(remainNano, TimeUnit.NANOSECONDS)
      f(aget, bget)
    }
    override def isCancelled = af.isCancelled || bf.isCancelled
    override def cancel(mayInterruptIfRunning: Boolean) = af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] ={
    ps.foldRight(unit(List[A]()))((pa, pl) => map2(pa, pl)(_ :: _))
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val lpo: List[Par[Option[A]]] = as.map(asyncF((a: A) => if (f(a)) Some(a) else None))
    map(sequence(lpo))(lo => lo.foldRight(List[A]())((o, al) => o match {
      case Some(a) => a :: al
      case None => al
    }))
  }

  def reduce[A](z: A)(al: IndexedSeq[A])(f: (A, A) => A): Par[A] = fork {
//    if (al.size <= 1) unit(al.headOption.getOrElse(z))
//    else {
//      println(al.size)
//      Thread.sleep(3000)
//      val (l, r) = al.splitAt(al.size / 2)
//      map2(fork(reduce(z)(l)(f)), fork(reduce(z)(r)(f)))(f)
//    }
    reduce2(z)(al)(a => a)(f)
  }

  def reduce2[A, B](z: B)(al: IndexedSeq[A])(f: A => B)(g: (B, B) => B): Par[B] = fork {
    if (al.size <= 1) unit(al.headOption.map(f).getOrElse(z))
    else {
      println(al.size)
      val (l, r) = al.splitAt(al.size / 2)
      map2(fork(reduce2(z)(l)(f)(g)), fork(reduce2(z)(r)(f)(g)))(g)
    }
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val temp = map2(pa, pb)((a, b) => (c: C) => f(a, b, c))
    map2(temp, pc)((fc, c) => fc(c))
  }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val temp1 = map2(pa, pb)((a, b) => (c: C, d: D) => f(a, b, c, d))
    val temp2 = map2(temp1, pc)((fcd, c) => (d: D) => fcd(c, d))
    map2(temp2, pd)((fd, d) => fd(d))
  }

  def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
    p(es).get() == p2(es).get()
  }

  def delay[A](fa: => Par[A]): Par[A] = {
    es => fa(es)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
//    es => {
//      choices.lift(n(es).get()) match {
//        case Some(a) => a(es)
//        case None => throw new Exception("out of index")
//      }
//    }
    chooser(n)(i => choices.lift(i).get)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
//    val n: Par[Int] = map(cond)(cond => if (cond) 0 else 1)
//    val choices = List(t, f)
//    choiceN(n)(choices)
    chooser(cond)(b => if(b) t else f)
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es => {
      choices.get(key(es).get()) match {
        case Some(a) => a(es)
        case None => throw new Exception("not exist key")
      }
    }
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      choices(pa(es).get())(es)
    }
  }

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => a(es).get()(es)
//    flatMap(a)(pa => pa)
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
//    es => f(a(es).get())(es)
    join(map(a)(f))
  }
}
