package numerals

case class NumeralsConverter() {
  def apply(i: Int): String = {
    i match {
      case 0 => ""
      case 1 => "I"
      case 4 => "IV"
      case 9 => "IX"
      case n if n >= 10 => "X" + apply(i - 10)
      case n if n >= 5 => "V" + apply(i - 5)
      case n if n < 5 => "I" + apply(i - 1)
    }
  }
}
