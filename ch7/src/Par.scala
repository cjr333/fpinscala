import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout:Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((x, y) => map2(x, y)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

//  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
//    val fas: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
//    map(sequence(fas))(_.flatten)
//  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas: List[Par[Option[A]]] = as.map(asyncF((a: A) => if (f(a)) Some(a) else None))
    map(sequence(fas))(_.foldRight[List[A]](List())((x, y) => x match {
      case Some(_) => x.get :: y
      case _ => y
    }))
  }
}

case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                             f: (A,B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None
  def isDone = cache.isDefined
  def isCancelled = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean) =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime;val aTime = stop-start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}

