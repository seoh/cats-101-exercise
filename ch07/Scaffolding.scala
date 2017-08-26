/**
 * 7.1.3
 */
object Scaffolding extends App {
  // implment map, flatMap, filter, sum using foldRight

  def map[A, B](list: List[A])(fn: A => B): List[B] =
    list.foldRight(List.empty[B])((i, acc) => fn(i) :: acc)

  def flatMap[A, B](list: List[A])(fn: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((i, acc) => fn(i) ++ acc)

  def filter[A](list: List[A])(fn: A => Boolean): List[A] =
    list.foldRight(List.empty[A])((i, acc) => if(fn(i)) i :: acc else acc)

  def sum[A](list: List[A])(implicit n: Numeric[A]): A =
    list.foldRight(n.zero)(n.plus(_, _))

  
  val xs = List(1,2,3,4)
  assert(xs.map(_.toString) == map(xs)(_.toString))
  assert(xs.flatMap(x => List.fill(x)(x)) == flatMap(xs)(x => List.fill(x)(x)))
  assert(xs.filter(_ % 2 == 0) == filter(xs)(_ % 2 == 0))
  assert(xs.sum == sum(xs))
  assert(sum(List(0.1, 0.2, 0.3)) == 0.6)
}