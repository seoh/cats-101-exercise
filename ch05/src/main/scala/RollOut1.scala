/**
 * 5.3
 */
object RollOut1 extends App {
  import cats.data.EitherT
  import scala.concurrent.Future

  type Response[A] = EitherT[Future, String, A]
}