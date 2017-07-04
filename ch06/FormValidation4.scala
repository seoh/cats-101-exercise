/**
 * 6.4.4
 */
object FormValidation4 extends App {
  import cats.syntax.either._

  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ErrorOn[A] = Either[List[String], A]


  def getValue(name: String)(form: FormData): ErrorOn[String] =
    form.get(name) match {
      case Some(value) => value.asRight
      case None => List(s"$name is not found.").asLeft
    }

  def parseInt(str: String): ErrorOn[Int] =
    Either
      .catchOnly[NumberFormatException](str.toInt)
      .leftMap(error => List(error.toString))


  def nonBlank(str: String): ErrorOn[String] =
    if(str.isEmpty) List("String must be non-empty.").asLeft
    else str.asRight
  
  def nonNegative(num: Int): ErrorOn[Int] =
    if(num < 0) List(s"$num must be positive.").asLeft
    else num.asRight


  def readName(form: FormData): ErrorOn[String] =
    for {
      name <- getValue("name")(form)
      checked <- nonBlank(name)
    } yield checked
    
  def readAge(form: FormData): ErrorOn[Int] =
    for {
      age <- getValue("age")(form)
      parsed <- parseInt(age)
      checked <- nonNegative(parsed)
    } yield checked

  println(readName(Map("name" -> "Name")))
  println(readName(Map("Name" -> "Name")))

  println(readAge(Map("age" -> "1")))
  println(readAge(Map("age" -> "-1")))
  println(readAge(Map("age" -> "a")))
  println(readAge(Map.empty))

}