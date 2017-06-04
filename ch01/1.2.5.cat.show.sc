import $ivy.`org.typelevel::cats:0.9.0`, cats._

import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

final case class Cat(
  name: String,
  age: Int,
  color: String
)

implicit val catShow: Show[Cat] = Show.show { cat =>
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show

  s"$name is a $age year-old $color cat."
}

println(Cat("nabi", 15, "black").show)
