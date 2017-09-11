/**
 * 10.4.1. Not exercise
 */

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._

import cats.syntax.semigroup._  // |+|
import cats.syntax.cartesian._  // |@|

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)
  
  def or(that: Predicate[E, A]): Predicate[E, A] =
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

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Pure[E, A](fn: A => Validated[E, A]) extends Predicate[E, A]
