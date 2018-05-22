package ch7

import Par._
import java.util.concurrent._

object util {
  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(1000)

    val is1 = IndexedSeq[Int](1, 2, 3, 4, 5, 6, 7, 8)
    val start = System.nanoTime()
    val sum = reduce(0)(is1)(_ + _)(es).get()
    val end = System.nanoTime()
    println("result : " + sum + ", elapsed time : " + TimeUnit.MILLISECONDS.convert(end - start, TimeUnit.NANOSECONDS))

    val max = reduce(Int.MinValue)(is1)(Math.max(_, _))(es).get()
    println("result : " + max)

    val sl = List("a b c", "d e", "f g h i")
    val wordsSum = reduce2(0)(sl.toIndexedSeq)(_.split(' ').length)(_ + _)(es).get()
    println("result : " + wordsSum)

    val map3Par = map3(unit("a"), unit("bc"), unit("def"))((s1, s2, s3) => s1.length() + s2.length() * s3.length())
    println("result: " + map3Par(es).get())
  }
}
