/**
 * 7.1.2
 */
object ReflectingFold extends App {
  def left[A](list: List[A]): List[A] =
    list.foldLeft(List.empty[A])((acc, i) => i :: acc)

  def right[A](list: List[A]): List[A] =
    list.foldRight(List.empty[A])((i, acc) => i :: acc)

  val xs = List(1,2,3,4)
  println(left(xs))   // List(4, 3, 2, 1)
  println(right(xs))  // List(1, 2, 3, 4)
}
