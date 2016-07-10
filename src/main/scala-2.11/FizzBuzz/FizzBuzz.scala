package fizzbuzz

class FizzBuzz {

  var value = 0
  var report = ""
  var result = ""


  def apply(value: Int): String = {
    using(value) {
      check3 _ andThen check5 andThen check6 andThen check4
    } orToStringIfNeither
  }

  def check3(fizz: FizzBuzz): FizzBuzz = fizz report "fizz" when isDivisibleBy(3)

  def check5(fizz: FizzBuzz): FizzBuzz = fizz report "buzz" when isDivisibleBy(5)

  def check6(fizzBuzz: FizzBuzz): FizzBuzz = fizzBuzz report "burp" when isDivisibleBy(6)

  def check4(fizzBuzz: FizzBuzz): FizzBuzz = fizzBuzz report "quack" when isDivisibleBy(4)

  def isDivisibleBy(divisor: Int): (Int) => Boolean = {
    value => value % divisor == 0
  }

  def using(aValue: Int)(fn: (FizzBuzz) => FizzBuzz) = {
    value = aValue
    fn(this)
  }

  def report(toReport: String): FizzBuzz = {
    report = toReport
    this
  }

  def when(test: (Int) => Boolean): FizzBuzz = {
    if (test(value)) {
      result += report
    }
    this
  }

  def orToStringIfNeither: String = if (result.isEmpty) value.toString else result
}
