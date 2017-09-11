/**
 * 10.4.2
 */
object Check3 extends App {
  import cats._
  import cats.data.Validated

  import cats.syntax.either._

  sealed trait Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]
    def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)
    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, f)
    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
  }

  final case class AndThen[E, A, B, C](
    check1: Check[E, A, B],
    check2: Check[E, B, C]
  ) extends Check[E, A, C] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(in).withEither(_.flatMap(b => check2(b).toEither))
  }

  final case class FlatMap[E, A, B, C](
    check: Check[E, A, B],
    func: B => Check[E, A, C]
  ) extends Check[E, A, C] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).withEither(_.flatMap(b => func(b)(in).toEither))
  }

  final case class Map[E, A, B, C](
    check: Check[E, A, B],
    func: B => C
  ) extends Check[E, A, C] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(func)
  }

  final case class Pure[E, A](
    pred: Predicate[E, A]
  ) extends Check[E, A, A] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
  }
}