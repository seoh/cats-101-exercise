/**
 * 4.9.1
 */
object BranchingOutWithMonad extends App {
  import cats.Monad

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad = new Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case Leaf(a) => fn(a)
      case Branch(l, r) => branch(flatMap(l)(fn), flatMap(r)(fn))
    }

    def pure[A](a: A): Tree[A] = leaf(a)

    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = fn(a) match {
      case Leaf(Right(b)) => leaf(b)
      case Leaf(Left(a)) => tailRecM(a)(fn)
      case Branch(l, r) => branch(
        flatMap(l) {
          case Left(a) => tailRecM(a)(fn)
          case Right(b) => pure(b)
        },
        flatMap(r) {
          case Left(a) => tailRecM(a)(fn)
          case Right(b) => pure(b)          
        }
      )
    }
  }

  val tree: Tree[Int] =
    branch(
      branch(leaf(1), leaf(2)),
      leaf(3)
    )
  
  println(treeMonad.map(tree)(_ + 1))
}