/**
 * 7.2.2.2
 */
object TraversingValidated extends App {
  import scala.language.higherKinds

  import cats._
  import cats.syntax.cartesian._
  import cats.syntax.applicative._
  import cats.instances.vector._
  

  def listTraverse[F[_] : Applicative, A, B]
      (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum |@| func(item)).map(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
      (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)



  import cats.data.Validated
  import cats.instances.list._ // Applicative[ErrorsOr] needs a Monoid[List]

  type ErrorsOr[A] = Validated[List[String], A]
  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  println(process(List(2, 4, 6))) // Valid(List(2, 4, 6))
  println(process(List(1, 2, 3))) // Invalid(List(1 is not even, 3 is not even))
}
