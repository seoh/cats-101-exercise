/**
 * 6.3.4
 */
object CartesianMonad extends App {
  import scala.language.higherKinds
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def product[M[_]: Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] = 
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  import cats.instances.list._
  println(product(List(1, 2), List(3, 4)))
}