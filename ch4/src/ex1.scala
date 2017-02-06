object ex1 {
  def main(args: Array[String]): Unit = {
    println(mean(Seq(1.0, 2.0, 3.0)).map(d => d.toInt))
    println(mean(Seq.empty[Double]).map(d => d.toInt))
    println(Some(4).map(tenDividedBy))
    //println(Some(0).map(tenDividedBy))
    println(None.map(tenDividedBy))
    println(Some(4).flatMap(tenDividedByFlat))
    println(Some(0).flatMap(tenDividedByFlat))
    println(None.flatMap(tenDividedByFlat))
    println(Some(2).filter(_ % 2 == 1))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def tenDividedBy(n: Int): Double = {
    if(n == 0) throw new Exception("divided by 0")
    else 10.0/n
  }

  def tenDividedByFlat(n: Int): Option[Double] = {
    if(n == 0) None
    else Some(10.0/n)
  }
}
