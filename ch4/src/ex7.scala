import Either.sequence
import Either.traverse

object ex7 {
  def main(args: Array[String]): Unit = {
    println(sequence(List(Right(1), Left("First error"), Right(2), Left("Second error"))))
    println(traverse(List("1", "a", "2", "k"))(s => Try(s.toInt)))
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}
