/**
 * 5.3
 */
object RollOut3 extends App {
  import cats.data.EitherT
  import cats.syntax.applicative._
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.future._
  import cats.instances.either._

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  // Why not `.asLeftT` or `.asRightT` on `EitherT`, like `Either`
  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(level) => EitherT.right(Future(level))
    case None => EitherT.left(Future("Unreachable"))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      res1 <- getPowerLevel(ally1)
      res2 <- getPowerLevel(ally2)
    } yield (res1 + res2) > 15
  

  def test(ally1: String, ally2: String) =
    canSpecialMove(ally1, ally2)
    .value
    .foreach(r => println(s"$ally1 + $ally2 > 15 :: $r"))

  test("Jazz", "Bumblebee")
  test("Jazz", "Hot Rod")
  test("Jazz", "jazz")
}