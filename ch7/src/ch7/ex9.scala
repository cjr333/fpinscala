package ch7

import Par._
import java.util.concurrent._

object ex9 {
  def main(args: Array[String]): Unit = {
    val a = lazyUnit(42 + 1)
    val b = delay(unit(42 + 1))
    val S = Executors.newFixedThreadPool(1)
//    println(Thread.currentThread().getId())
//    println(fork(a)(S).get())
//    println(b(S).get())

    val a2 = MyPar.fork(MyPar.fork(MyPar.unit(42 + 1)))
    println(MyPar.fork(MyPar.fork(MyPar.fork(MyPar.fork(MyPar.fork(a2)))))(S).apply(i => println(i)))
  }
}
