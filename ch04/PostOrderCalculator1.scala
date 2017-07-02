/**
 * 4.8.3
 */
object PostOrderCalculator1 extends App {
  
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

  try evalOne("a")
  catch { case t: Throwable => println(t.getMessage) }

  println(evalOne("42").run(Nil).value)
  println(evalOne("0").run(List(1, 2)).value)
  println(evalOne("+").run(List(1, 2)).value)
}