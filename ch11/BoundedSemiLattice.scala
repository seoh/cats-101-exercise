/**
 * 11.3.2.1
 */
object BoundedSemiLattice extends App {

  import cats.Monoid
  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  val intBSL = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty: Int = 0
  }

  def setBSL[T] = new BoundedSemiLattice[Set[T]] {
    def combine(s1: Set[T], s2: Set[T]): Set[T] = s1 union s2
    def empty: Set[T] = Set.empty[T]
  }

  
}