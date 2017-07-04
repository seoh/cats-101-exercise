/**
 * 6.4.4
 */
object FormValidation5 extends App {
  import cats.data.Validated

  import cats.instances.either._
  import cats.instances.list._
  import cats.instances.map._
  import cats.instances.string._

  import cats.syntax.cartesian._
  import cats.syntax.either._


  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type ErrorOn[A] = Either[List[String], A]
  type AllErrorsOr[A] = Validated[List[String], A]


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
    str.asRight.ensure(List("String must be non-empty."))(_.nonEmpty)
  
  def nonNegative(num: Int): ErrorOn[Int] =
    num.asRight.ensure(List(s"$num must be positive."))(_ >= 0)


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

  def readUser(form: FormData): AllErrorsOr[User] =
    (
      readName(form).toValidated |@| 
      readAge(form).toValidated
    ).map(User.apply)
  
  
  println(readUser(Map("name" -> "Name", "age" -> "1")))
  println(readUser(Map("name" -> "", "age" -> "-1")))
  println(readUser(Map("name" -> "Name", "age" -> "age")))
}