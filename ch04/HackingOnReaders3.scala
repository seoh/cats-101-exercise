/**
 * 4.7.3
 */
object HackingOnReaders3 extends App {
  import cats.data.Reader

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  val userReader: DbReader[Map[Int, String]] =
    Reader(db => db.usernames)

  val passReader: DbReader[Map[String, String]] =
    Reader(db => db.passwords)

  def findUsername(userId: Int): DbReader[Option[String]] =
    userReader.map(_.get(userId))

  def checkPassword(
    username: String,
    password: String
  ): DbReader[Boolean] = 
    passReader.map(_.get(username) == Some(password))


  import cats.syntax.applicative._

  val falsyReader = false.pure[DbReader]
  def checkLogin(
    userId: Int,
    password: String
  ): DbReader[Boolean] = 
    findUsername(userId) flatMap {
      case None => falsyReader
      case Some(u) => checkPassword(u, password)
    }

  val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ),
    Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
  )

  println(checkLogin(1, "zerocool").run(db)) // true
  println(checkLogin(4, "davinci").run(db))  // false
}