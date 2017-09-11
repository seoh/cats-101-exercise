/**
 * 10.3
 */
object BasicCombinator3 extends App {

  import cats._
  import cats.syntax.either._
  import cats.syntax.semigroup._

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
      case Pure(fn) => fn(a)
      case And(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e1), _) => e1.asLeft
          case (_, Left(e2)) => e2.asLeft
          case _ => a.asRight
        }
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Pure[E, A](fn: A => Either[E, A]) extends Check[E, A]

  /////////////////////////////////////////////////////////////////////////////////////

  import cats.instances.list._

  val a = Pure { (v: Int) =>
    if(v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val b = Pure { (v: Int) =>
    if(v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val check = a and b

  println(check(5)) // Left(List(Must be < -2))
  println(check(0)) // Left(List(Must be > 2, Must be < -2))
}