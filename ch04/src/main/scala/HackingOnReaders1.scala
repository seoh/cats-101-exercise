/**
 * 4.7.3
 */
object HackingOnReaders1 extends App {
  import cats.data.Reader

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  val userReader: DbReader[Map[Int, String]] =
    Reader(db => db.usernames)

  val db = Db(
    Map(1 -> "a", 2 -> "b"),
    Map("a" -> "aa", "b" -> "bb")
  )

  println(userReader.run(db).get(1))
  println(userReader.run(db).get(0))
}