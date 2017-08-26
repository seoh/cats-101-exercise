

name := "cats-101-exercise"
version := "0.0.1"
scalaVersion in Global := "2.12.3"

libraryDependencies in Global ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)

lazy val ch01 = project in file("ch01")
lazy val ch02 = project in file("ch02")
lazy val ch03 = project in file("ch03")
lazy val ch04 = project in file("ch04")
lazy val ch05 = project in file("ch05")
lazy val ch06 = project in file("ch06")
lazy val ch07 = project in file("ch07")
lazy val ch08 = project in file("ch08")
lazy val ch09 = project in file("ch09")
