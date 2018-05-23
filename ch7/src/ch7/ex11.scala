package ch7

import java.util.concurrent.Executors

import Par._

object ex11 {
  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(10)

    val listChoiced = choiceN(unit(3))(List(unit(1), unit(3), unit(5), unit(7), unit(9)))
    println(listChoiced(es).get())

    val trueOrFalse = choice(unit(true))(unit(true), unit(false))
    println(trueOrFalse(es).get())

    val mapChoiced = choiceMap(unit("b"))(Map("a" -> unit(1), "b" -> unit(2), "c" -> unit(3)))
    println(mapChoiced(es).get())
  }
}
