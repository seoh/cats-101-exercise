/**
  * 4.1.2
  */
object FuncY extends App {

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  val listMonad = new Monad[List] {
    def pure[A](a: A) = List(a)

    def flatMap[A, B](value: List[A])(func: A => List[B]) = value.flatMap(func)
  }

  println(
    listMonad.pure(1) == List(1)
  )

  println(
    listMonad.flatMap(List(1))(a => List(a, a)) == List(1, 1)
  )

  println(
    listMonad.map(List(1))(_ + 1) == List(2)
  )
}
