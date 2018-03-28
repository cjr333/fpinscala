case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object ex8 {
  def mkName(name: String): Either[String, Name] =
    if(name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] =
    if(age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  def mkPerson2(name: String, age: Int): Either[String, Person] =
    mkName(name).map2_2(mkAge(age))(Person)(_ + " (and) " + _)

  def main(args: Array[String]): Unit = {
    println(mkPerson("nexon", 26))
    println(mkPerson("", 26))
    println(mkPerson("nexon", -1))
    println(mkPerson("", -1))

    println("==========================================")

    println(mkPerson2("nexon", 26))
    println(mkPerson2("", 26))
    println(mkPerson2("nexon", -1))
    println(mkPerson2("", -1))
  }
}
