/**
  * 2.5.5
  */
object AllTheThings2 extends App {
  def add(items: List[Option[Int]]): Int =
    items.fold(Some(0)) { (acc, item) =>
      for {
        a <- acc
        i <- item
      } yield a + i
    }.getOrElse(0)

  def add2(items: List[Option[Int]]): Int =
    items.map(_.getOrElse(0)).sum

  import cats._
  import cats.instances.option._
  import cats.instances.int._
  import cats.syntax.semigroup._

  def add3(items: List[Option[Int]]): Int =
    items.foldLeft(Monoid[Option[Int]].empty)(_ |+| _).getOrElse(0)

  println(
    add(List(Some(1), Some(2), Some(3))),
    add(List(Some(1), None, Some(3))),
    add(List.empty[Option[Int]])
  )
  println(
    add2(List(Some(1), Some(2), Some(3))),
    add2(List(Some(1), None, Some(3))),
    add2(List.empty[Option[Int]])
  )
  println(
    add3(List(Some(1), Some(2), Some(3))),
    add3(List(Some(1), None, Some(3))),
    add3(List.empty[Option[Int]])
  )

}
