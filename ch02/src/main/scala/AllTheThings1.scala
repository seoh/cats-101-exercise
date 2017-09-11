/**
  * 2.5.5
  */
object AllTheThings1 extends App {

  def add(items: List[Int]): Int = items.sum

  println(add(List(1, 2, 3)))

  println(add(List.empty[Int]))

}
