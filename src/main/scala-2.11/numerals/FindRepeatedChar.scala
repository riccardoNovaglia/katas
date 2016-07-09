package numerals

class FindRepeatedChar() {
  def find(searchIn: String, searched: Char): Option[String] = {
    if (containsDouble(searchIn, searched)) {
      val upToSearched = searchIn.drop(searchIn.indexOf(searched))
      Some(upToSearched.dropRight(upToSearched.reverse.indexOf(searched)))
    } else {
      None
    }
  }

  def containsDouble(searchIn: String, searched: Char): Boolean = {
    searchIn.count(_ == searched) > 1
  }
}

object findRepeated {
  var searchedFor: Char = _

  def apply(searched: Char) = {
    searchedFor = searched
    this
  }

  def in(searchIn: String): Option[String] = {
    new FindRepeatedChar().find(searchIn, searchedFor)
  }
}

