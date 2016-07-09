package fizzbuzz

class FizzBuzz {

  implicit def intToDivisible(int: Int): IsDivisible = new IsDivisible(int)

  def apply(value: Int): String = {
    (value isDivisibleBy 3, value isDivisibleBy 5) match {
      case (true, true) => "fizzbuzz"
      case (true, _) => "fizz"
      case (_, true) => "buzz"
      case _ => value.toString
    }
  }
}

case class IsDivisible(value: Int) {
  def isDivisibleBy(divisor: Int) = value % divisor == 0
}
