import List._

object ex19 {
  def main(args: Array[String]): Unit = {
    println(filter(List(1, 2, 3, 4))(x => (x % 2) == 0))
  }
}
