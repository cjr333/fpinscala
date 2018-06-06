import java.util.concurrent.Executors

import Gen._
import Prop._
import ch7.Par.Par

object Prop {
  type FailedCase = String
  type SucessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
//      val prop: Prop =
//        props.map(p => Prop { (max, _, rng) =>
//          p.run(max, casesPerSize, rng)
//        }).toList.reduce(_ && _)
//      prop.run(max, n, rng)
      props.map(p => p.run(max, casesPerSize, rng)).toList.reduce(_ && _)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => scala.Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val S = weighted(
    choose(1, 5).map(Executors.newFixedThreadPool) ->   .75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(S ** g) { case (s, a) => f(a)(s).get }
  }

  def checkPar(p: => Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  sealed trait Result {
    def isFalsified: Boolean
    def &&(r: Result): Result
    def ||(r: Result): Result
  }
  case object Passed extends Result {
    override def isFalsified = false
    override def &&(r: Result): Result = r match {
      case Falsified(f, s) => Falsified("Right : " + f, s)
      case _ => Passed
    }
    override def ||(r: Result): Result = r match {
      case Proved => Proved
      case _ => Passed
    }
  }
  case class Falsified(failure: FailedCase, successes: SucessCount) extends Result {
    override def isFalsified = true
    override def &&(r: Result): Result = {
      Falsified("Left : " + this.failure, this.successes)
    }
    override def ||(r: Result): Result = r match {
      case Falsified(f, s) => Falsified("Left : " + this.failure + ", Right : " + f, Math.min(this.successes, s))
      case _ => r
    }
  }
  case object Proved extends Result {
    override def isFalsified = false
    override def &&(r: Result): Result = r match {
      case Proved => Proved
      case Passed => Passed
      case Falsified(f, s) => Falsified("Right : " + f, s)
    }
    override def ||(r: Result): Result = this
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) && p.run(max, n, rng)
  }
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) || p.run(max, n, rng)
  }
}
