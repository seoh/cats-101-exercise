/**
 * 11.2.3
 */
object GCounter extends App {
  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter = {
      val prev = counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> (prev + amount)))
    }

    def get: Int =
      counters.values.sum
  
    def merge(that: GCounter): GCounter = {
      val keys = counters.keySet ++ that.counters.keySet
      val counter = (keys map { key =>
        val amount = math.max(
          this.counters.getOrElse(key, 0),
          that.counters.getOrElse(key, 0)
        )
        (key -> amount)
      }).toMap
      GCounter(counter)
    }
  }

  val a = GCounter(Map("A" -> 0, "B" -> 0))
  val b = GCounter(Map("A" -> 0, "B" -> 0))

  val a2 = a.increment("A", 3)
  val b2 = b.increment("B", 2) 

  println(a2)
  println(b2)

  val merged = a2 merge b2
  println(merged)

  println(s"ALL: ${merged.get}")
}