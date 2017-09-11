/**
 * 6.4.4
 */
object FormValidation2 extends App {
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


  println(parseInt("111"))
  println(parseInt("aaa"))
}