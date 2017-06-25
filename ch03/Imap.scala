/**
  * 3.6.2.1
  */
object Imap extends App {

  import scala.util.Try

  trait Codec[A] {
    def encode(value: A): String

    def decode(value: String): Option[A]

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        def encode(value: B): String = self.encode(enc(value))

        def decode(value: String): Option[B] = self.decode(value).map(dec)
      }
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)

  implicit val intCodec = new Codec[Int] {
    def encode(value: Int): String = value.toString

    def decode(value: String): Option[Int] = Try(value.toInt).toOption
  }

  println(intCodec.decode("a"))
  println(intCodec.decode(intCodec.encode(1)))

  val stringCodec = intCodec.imap[String](_.toString, _.toInt)


  case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)

  println(encode(Box(123)))
  println(decode[Box[Int]]("123"))
}
