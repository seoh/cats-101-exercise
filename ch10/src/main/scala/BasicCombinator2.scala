/**
 * 10.3
 */
object BasicCombinator2 {

  import cats._

  // evaluate lazily for short-circuit evalution. isnt it?
  trait Check[E, A] {
    def and(that: => Check[E, A])(implicit s: Semigroup[E]): Check[E, A] =
      ???
  }
}