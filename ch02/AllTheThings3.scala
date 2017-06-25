/**
  * 2.5.5
  */
object AllTheThings3 extends App {

  import cats._
  import cats.syntax.semigroup._

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new Monoid[Order] {
    def empty = Order(0, 0)

    def combine(o1: Order, o2: Order) = Order(
      o1.totalCost + o2.totalCost,
      o1.quantity + o2.quantity
    )
  }

  def add(items: List[Order]): Order =
    items.foldLeft(Monoid[Order].empty)(_ |+| _)


  /**
  compile error
  ```
  could not find implicit value for parameter ev: cats.kernel.Monoid[T]
  ```

  why????
    */
  // def add[T](items: List[T]): T =
  //   items.foldLeft(Monoid[T].empty)(_ |+| _)

  println(add(List(
    Order(1, 1),
    Order(2, 2),
    Order(3, 3)
  )))

}
