/**
 * 9.3.3
 */
object ParallelFoldMap extends App {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  val CPUs = Runtime.getRuntime.availableProcessors

  import cats._
  import cats.syntax.semigroup._
  import cats.syntax.traverse._
  import cats.instances.list._
  import cats.instances.future._

  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B = 
    values map func reduce (_ |+| _)

  def parallelFoldMap[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] = {
      values.grouped(CPUs).toList
        .map(as => Future(as map func))
        .map(bsF => bsF map (bs => bs.reduce(_ |+| _)))
        .sequence
        .map(bs => bs.reduce(_ |+| _))
    }

  import cats.instances.int._
  import scala.concurrent.Await
  import scala.concurrent.duration._

  println(
    Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1.second)
  )
}