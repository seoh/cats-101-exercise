/**
 * 9.2
 */
object FoldMap2 extends App {
  import cats._
  import cats.syntax.semigroup._

  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B = {
    // 1
    val as: Vector[A] = values
    // 2
    val bs: Vector[B] = as map func
    // 3
    bs reduce (_ |+| _)
  }


  import cats.instances.int._
  import cats.instances.string._

  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
}