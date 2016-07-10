package fizzbuzz

class FizzBuzz {

  implicit def intToDivisible(int: Int): IsDivisible = new IsDivisible(int)

  def apply(value: Int): String = {
    value report "fizz" when isDivisibleBy(3) report "buzz" when isDivisibleBy(5) orToStringIfNeither
  }

  def isDivisibleBy(divisor: Int): (Int) => Boolean = {
    value => value % divisor == 0
  }
}

case class IsDivisible(value: Int) {
  var report: String = _
  var result: String = ""

  def report(toReport: String): IsDivisible = {
    report = toReport
    this
  }

  def when(test: (Int) => Boolean): IsDivisible = {
    if (test(value)) {
      result += report
    }
    this
  }

  def orToStringIfNeither: String = if (result.isEmpty) value.toString else result
}
