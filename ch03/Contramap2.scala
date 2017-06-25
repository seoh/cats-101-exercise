/**
  * 3.6.1.1
  */
object Contramap2 extends App {

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

  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  final case class Box[A](value: A)

  /* // Dumb Solution //
  *
  * implicit val booleanBox =
  *   booleanPrintable.contramap[Box[Boolean]](b => b.value)
  *
  * implicit val stringBox =
  *   stringPrintable.contramap[Box[String]](b => b.value)
  *
  */

  implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  println(format(Box("hello world")))
  println(format(Box(true)))

}
