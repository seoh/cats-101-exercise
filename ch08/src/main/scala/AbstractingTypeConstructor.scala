/**
 * 8.1
 */
object AbstractingTypeConstructor extends App {
  import cats._
  import scala.concurrent.Future
  
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int]
  }
}
