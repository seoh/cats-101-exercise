/**
 * 11.3.2.2
 */
object GenericGCounter extends App {
  import cats.Monoid
  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  implicit val intBSL = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty: Int = 0
  }

  implicit def setBSL[T] = new BoundedSemiLattice[Set[T]] {
    def combine(s1: Set[T], s2: Set[T]): Set[T] = s1 union s2
    def empty: Set[T] = Set.empty[T]
  }


  final case class GCounter[T](counters: Map[String, T]) {
    def increment(machine: String, amount: T)(implicit t: BoundedSemiLattice[T]): GCounter[T] = {
      val prev = counters.getOrElse(machine, t.empty)
      GCounter(counters + (machine -> t.combine(prev, amount)))
    }

    def get(implicit t: BoundedSemiLattice[T]): T =
      counters.values.foldLeft(t.empty)(t.combine)
  
    def merge(that: GCounter[T])(implicit t: BoundedSemiLattice[T]): GCounter[T] = {
      val keys = counters.keySet ++ that.counters.keySet
      val counter = (keys map { key =>
        val amount = t.combine(
          this.counters.getOrElse(key, t.empty),
          that.counters.getOrElse(key, t.empty)
        )
        (key -> amount)
      }).toMap
      GCounter(counter)
    }
  }

  // val a = GCounter(Map("A" -> 0, "B" -> 0))
  // val b = GCounter(Map("A" -> 0, "B" -> 0))

  // val a2 = a.increment("A", 3)
  // val b2 = b.increment("B", 2) 

  // println(a2)
  // println(b2)

  // val merged = a2 merge b2
  // println(merged)

  // println(s"ALL: ${merged.get}")

  val a = GCounter(Map("A" -> Set(1, 2)))
  val b = GCounter(Map("B" -> Set(10, 20)))

  val a2 = a.increment("A", Set(3))
  val b2 = b.increment("B", Set(30)) 

  println(a2)
  println(b2)

  val merged = a2 merge b2
  println(merged)

  println(s"ALL: ${merged.get}")

}