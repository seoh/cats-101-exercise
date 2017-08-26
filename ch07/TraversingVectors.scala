/**
 * 7.2.2.1
 */
object TraversingVectors extends App {
  import scala.language.higherKinds

  import cats._
  import cats.syntax.cartesian._
  import cats.syntax.applicative._
  import cats.instances.vector._
  

  def listTraverse[F[_] : Applicative, A, B]
      (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum |@| func(item)).map(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
      (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  println(
    listSequence(List(Vector(1, 2), Vector(3, 4)))
    // List(Vector(1, 2), Vecot(3, 4)) 
    //   =>
    // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  )

  println(
    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
    // List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
    //   =>
    // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6),
    //        List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
  )
}
