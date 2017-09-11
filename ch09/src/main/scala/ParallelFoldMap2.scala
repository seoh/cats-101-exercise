/**
 * 9.3.4
 */
object ParallelFoldMap2 extends App {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  val CPUs = Runtime.getRuntime.availableProcessors

  import cats._
  import cats.syntax.semigroup._
  import cats.syntax.traverse._
  import cats.syntax.foldable._

  import cats.instances.list._
  import cats.instances.future._
  import cats.instances.vector._

  def parallelFoldMap[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] =
      values.grouped(CPUs).toList
        .traverse(g => Future(g.foldMap(func)))
        .map(_.combineAll)

  import cats.instances.int._
  import scala.concurrent.Await
  import scala.concurrent.duration._

  println(
    Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1.second)
  )
}