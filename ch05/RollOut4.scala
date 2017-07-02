/**
 * 5.3
 */
object RollOut4 extends App {
  import cats.data.EitherT
  import cats.syntax.applicative._
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.future._
  import cats.instances.either._

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Await
  import scala.concurrent.duration._

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  // Why not `.asLeftT` or `.asRightT` on `EitherT`, like `Either`
  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(level) => EitherT.right(Future(level))
    case None => EitherT.left(Future(s"$autobot unreachable"))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      res1 <- getPowerLevel(ally1)
      res2 <- getPowerLevel(ally2)
    } yield (res1 + res2) > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 5.seconds) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Jazz", "Hot Rod"))
  println(tacticalReport("Jazz", "jazz"))
}