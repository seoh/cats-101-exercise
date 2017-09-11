/**
 * 8.2
 */
object AbstractingMonad extends App {
  import cats._
  import cats.instances.list._
  import scala.concurrent.Future
  
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  class UptimeService(client: UptimeClient[Future]) {
    def getTotalUptime(hostnames: List[String]): Future[Int] = ???
  }
}
