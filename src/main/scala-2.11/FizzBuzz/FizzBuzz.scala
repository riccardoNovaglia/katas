package fizzbuzz

class FizzBuzz {
  def apply(value: Int): String = {
    var toFizBuz: String = ""
    if (value % 3 == 0) toFizBuz += "fizz"
    if (value % 5 == 0) toFizBuz += "buzz"

    if (toFizBuz == "") value.toString else toFizBuz
  }

}
