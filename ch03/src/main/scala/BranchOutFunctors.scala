/**
  * 3.5.4
  */
object BranchOutFunctors extends App {

  import cats._
  import cats.syntax.functor._


  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
      case Branch(l, r) => Branch(map(l)(func), map(r)(func))
      case Leaf(value) => Leaf(func(value))
    }
  }

  val tree: Tree[Int] = Branch(
    Leaf(1),
    Branch(
      Branch(
        Branch(
          Leaf(2),
          Leaf(3)
        ),
        Leaf(4)
      ),
      Leaf(5)
    )
  )

  // println(treeFunctor.map(tree)(_ + 1))
  println(tree.map(_ + 1))

}
