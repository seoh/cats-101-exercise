/**
  * 2.3
  */
object BooleanMonoid extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }

  implicit val andMonoid = new Monoid[Boolean] {
    def empty = false

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val orMonoid = new Monoid[Boolean] {
    def empty = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val xorMonoid = new Monoid[Boolean] {
    def empty = false

    def combine(x: Boolean, y: Boolean): Boolean = x ^ y
  }

}
