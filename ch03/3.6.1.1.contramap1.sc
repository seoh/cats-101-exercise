
trait Printable[A] {
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = {
    val self = this
    new Printable[B] {
      def format(value: B): String =
        self.format(func(value))
    }
  }
}

def format[A](value: A)(implicit p: Printable[A]): String =
  p.format(value)

implicit val printableInt = new Printable[Int] {
  def format(value: Int) = value.toString
}

implicit val printableDouble =
  printableInt.contramap( (d: Double) => d.toInt )

println(format(1.5))

