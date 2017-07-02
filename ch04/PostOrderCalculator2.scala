/**
 * 4.8.3
 */
object PostOrderCalculator2 extends App {
  
  import cats.data.State

  type CalcState[A] = State[List[Int], A]
  
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operator(fn: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = fn(a, b)
        (ans :: tail, ans)
      case _ =>
        throw new Throwable("doh")
    }

  def operand(n: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (n :: stack, n)
    }

  import cats.syntax.applicative._
  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a flatMap (_ => evalOne(b))
    }

  println(evalAll(List("1", "2", "+", "3", "*")).run(Nil).value)
}