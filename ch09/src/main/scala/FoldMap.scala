/**
 * 9.2
 */
object FoldMap extends App {
  import cats._

  def foldMap[A: Monoid, B: Monoid](values: Vector[A])(func: A => B): B = ???
}