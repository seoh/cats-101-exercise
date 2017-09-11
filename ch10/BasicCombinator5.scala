/**
 * 10.3
 */
object BasicCombinator5 extends App {

  import cats._
  import cats.data.Validated
  import cats.data.Validated._

  import cats.instances.list._
  import cats.syntax.cartesian._
  import cats.syntax.semigroup._
  import cats.syntax.validated._


  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)
    
    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)
    
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(fn) => fn(a)
      case And(left, right) => (left(a) |@| right(a)) map ((_, _) => a)
      case Or(left, right) => left(a) match {
        case v @ Valid(a) => v
        case Invalid(e1) => right(a) match {
          case v @ Valid(a) => v
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Pure[E, A](fn: A => Validated[E, A]) extends Check[E, A]

  /////////////////////////////////////////////////////////////////////////////////////



  // infer from result type signature
  val a: Check[List[String], Int] = Pure { v =>
    if(v > 2) v.valid
    else List("Must be > 2").invalid
  }

  // or describe explicitly.
  val b = Pure { (v: Int) =>
    if(v < -2) v.valid[List[String]]
    else List("Must be < -2").invalid[Int]
  }

  val check = a and b

  println(check(5)) // Left(List(Must be < -2))
  println(check(0)) // Left(List(Must be > 2, Must be < -2))

  println((a or b)(5)) // Valid(5)
  println((a or b)(0)) // Invalid(List(Must be > 2, Must be < -2))
}