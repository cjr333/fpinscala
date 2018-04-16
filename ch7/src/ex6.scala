import java.util.concurrent.{ExecutorService, Executors}

object ex6 {
  def main(args: Array[String]): Unit = {
    val test: List[List[Int]] = List(List(1, 5), List(2), List(), List(4))
    println(test)
    println(test.flatten)

    val test2: List[Option[Int]] = List(Some(1), Some(2), None, Some(4))
    println(test2)
    println(test2.foldRight[List[Int]](List())((x, y) => x match {
      case Some(_) => x.get :: y
      case _ => y
    }))

    val executorService = Executors.newFixedThreadPool(10)

    val list = List(1, 2, 3, 4, 5)
    val parlist = Par.parMap(list)(i => {println(i); i.toDouble})
    println(parlist(executorService).get())

    val filterlist = Par.parFilter(list)(i => i % 2 == 0)
    println(filterlist(executorService).get())

  }
}
