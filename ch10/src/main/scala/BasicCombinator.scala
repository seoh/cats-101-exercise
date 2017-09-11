/**
 * 10.3
 */
object BasicCombinator {

  import cats._

  // use `Semigroup` to accumulate errors using dot operation.
  trait Check[E, A] {
    def and(that: Check[E, A])(implicit s: Semigroup[E]): Check[E, A] =
      ???
  }
}