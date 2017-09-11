/**
  * 1.1.4
  */
object Printable1 extends App {

  trait Printable[A] {
    def format(value: A): String
  }

  object Printable {
    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    def print[A](value: A)(implicit p: Printable[A]): Unit =
      println(format(value))
  }

  object PrintableInstances {
    implicit val printableString = new Printable[String] {
      override def format(value: String) = value
    }

    implicit val printableInt = new Printable[Int] {
      def format(value: Int) = value.toString
    }
  }


  import PrintableInstances._

  Printable.print("abc")
  Printable.print(12345)

}
