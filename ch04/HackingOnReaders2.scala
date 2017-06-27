/**
 * 4.7.3
 */
object HackingOnReaders2 extends App {
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

  implicit val db = Db(
    Map(1 -> "a", 2 -> "b"),
    Map("a" -> "aa", "b" -> "bb")
  )

  println(findUsername(1).run(db))
  println(findUsername(0).run(db))

  println(checkPassword("a", "aa").run(db))
  println(checkPassword("a", "bb").run(db))
}