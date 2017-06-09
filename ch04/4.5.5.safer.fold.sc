import $ivy.`org.typelevel::cats:0.9.0`

import cats._

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRight(as, Eval.now(acc))(fn).value


def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, B) => B): Eval[B] = as match {
  case x :: xs => Eval.defer(foldRight(xs, acc)(fn).map(b => fn(x, b)))
  case Nil => acc
}

println(
  foldRight[Int, BigInt]((1 to 50000).toList, 1)(_ * _).toString.length
)
