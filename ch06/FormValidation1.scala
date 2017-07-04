/**
 * 6.4.4
 */
object FormValidation1 extends App {
  import cats.syntax.either._

  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ErrorOn[A] = Either[List[String], A]


  def getValue(name: String)(form: FormData): ErrorOn[String] =
    form.get(name) match {
      case Some(value) => value.asRight
      case None => List(s"$name is not found.").asLeft
    }

  val form = Map("name" -> "value")

  println(getValue("name")(form))
  println(getValue("Name")(form))
}