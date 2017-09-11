/**
 * 8.2
 */
object AbstractingMonad2 extends App {
  import cats._
  import cats.syntax.traverse._
  import cats.syntax.functor._
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

  class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}
