/**
  * 1.1.4
  */
object Printable3 extends App {

  trait Printable[A] {
    def format(value: A): String
  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    def print[A](value: A)(implicit p: Printable[A]): Unit =
      println(format(value))
  }

  final case class Cat(
                        name: String,
                        age: Int,
                        color: String
                      )

  object PrintableInstances {
    implicit val printableString = new Printable[String] {
      override def format(value: String) = value
    }

    implicit val printableInt = new Printable[Int] {
      def format(value: Int) = value.toString
    }

    implicit val printableCat = new Printable[Cat] {
      def format(value: Cat) = {
        val name = Printable.format(value.name)
        val age = Printable.format(value.age)
        val color = Printable.format(value.color)

        s"$name is a $age year-old $color cat."
      }
    }
  }

  object PrintableSyntax {

    implicit class PrintOps[A](value: A) {
      def format(implicit p: Printable[A]): String = Printable.format(value)

      def print(implicit p: Printable[A]): Unit = Printable.print(value)
    }

  }

  import PrintableInstances._
  import PrintableSyntax._

  "abc".print
  12345.print
  Cat("nabi", 15, "black").print

  // Printable.print("abc")
  // Printable.print(12345)
  // Printable.print(Cat("nabi", 15, "black"))
}
