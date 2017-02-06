object ex3 {
  def main(args: Array[String]): Unit = {
    println(parseInsuranceRateQuote("35", "0"))
    println(parseInsuranceRateQuote("a", "0"))
    println(parseInsuranceRateQuote("35", "a"))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age / 30.0 + 1.2 * numberOfSpeedingTickets
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =  {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }
}
