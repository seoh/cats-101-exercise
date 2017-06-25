/**
  * 4.6.3
  */
object WriterFactorial extends App {

  import cats._
  import cats.data.Writer
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  /*
    Error occured like this `java.lang.NoClassDefFoundError: Could not initialize class ammonite.$file.ch04.4$u002E6$u002E3$u002Ewriter$u002Efactorial$`

    cause: https://github.com/lihaoyi/Ammonite/issues/534
  */
  //  interp configureCompiler { c =>
  //    c.settings.Ydelambdafy.value = "inline"
  //  }

  // type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(1)

  def factorial(n: Int): Int = {
    val ans = slowly {
      if (n == 0) 1
      else n * factorial(n - 1)
    }

    println(s"fact $n $ans")
    ans
  }

  val f = Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  ))

  // Await.result(f, Duration.Inf)
  Await.result(f, 10.seconds)
}
