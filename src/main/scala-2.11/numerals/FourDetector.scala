package numerals

class FourDetector {
  def get4Repeated(s: String) = {
    find4equalChars(s.toList)
  }

  private def find4equalChars(char: List[Char]): Option[String] = {
    char match {
      case xs if xs.length > 3 =>
        if (allElemsAreEqual(xs.take(4)))
          Some(xs.take(4).mkString)
        else
          find4equalChars(xs.drop(1))
      case xs if xs.length < 4 => None
    }
  }

  private def allElemsAreEqual(chars: List[Char]): Boolean = {
    chars.distinct.length == 1
  }
}
