/**
  * 2.4
  */
object SetMonoid extends App {

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

  implicit def setMonoid[T] = new Monoid[Set[T]] {
    def empty = Set.empty[T]

    def combine(x: Set[T], y: Set[T]): Set[T] = x ++ y
  }

  val set = setMonoid[Int]
  val empty = set.empty
  val one = Set(1)

  // () ++ (1) == (1) ++ () == (1)
  println(
    set.combine(empty, one) == set.combine(one, empty) &&
      set.combine(empty, one) == one &&
      set.combine(one, empty) == one
  )

  val two = Set(2)
  val others = Set(3, 4, 5)

  // {(1) ++ (2)} ++ (3,4,5) === (1) ++ {(2) ++ (3,4,5)}
  println(
    set.combine(set.combine(one, two), others) ==
      set.combine(one, set.combine(two, others))
  )
  
}
