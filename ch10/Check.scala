/**
 * 10.4.2
 */
object Check extends App {
  import cats._
  import cats.data.Validated

  sealed trait Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]
    def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
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