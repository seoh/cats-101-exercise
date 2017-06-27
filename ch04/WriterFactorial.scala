/**
  * 4.6.3
  */
object WriterFactorial extends App {

  import cats._
  import cats.data.Writer
  import cats.syntax.applicative._
  import cats.syntax.writer._

  import cats.instances.int._
  import cats.instances.vector._

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  /*
    Error occured like this 
    `java.lang.NoClassDefFoundError: Could not initialize class ammonite.$file.ch04$writer$factorial$`

    cause: https://github.com/lihaoyi/Ammonite/issues/534
  */
  //  // doesnt work
  //  interp configureCompiler { c =>
  //    c.settings.Ydelambdafy.value = "inline"
  //  }

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(1)

  def factorial(n: Int): Int = {
    def f(i: Int): Logged[Int] = {
      for {
        ans <- if(i == 0) 1.pure[Logged]
               else slowly { f(i - 1).map(_ * i) }
        _   <- Vector(s"$i $ans").tell 
      } yield ans
    }

    val (logs, result) = f(n).run
    logs.foreach(println)
    result
  }

  factorial(10)
}
