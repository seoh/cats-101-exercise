import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

type Id[A] = A

implicit val idMonad = new Monad[Id] {
  def pure[A](a: A): Id[A] = a
  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
}

println(idMonad.pure(1) == 1)
println(idMonad.flatMap(1)(_ + 1) == 2)
println(idMonad.map(1)(_ + 1) == 2)

import $ivy.`org.typelevel::cats-core:0.9.0`, cats._
import cats.syntax.functor._
import cats.syntax.flatMap._

val sum = for {
  a <- idMonad.pure(1)
  b <- idMonad.pure(2)
} yield a + b

println(sum == 3)

