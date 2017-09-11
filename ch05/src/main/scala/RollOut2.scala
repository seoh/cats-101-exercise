/**
 * 5.3
 */
object RollOut2 extends App {
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

  getPowerLevel("Jazz").value.foreach(println) // Right(6)
  getPowerLevel("jazz").value.foreach(println) // Left(Unreachable)
}