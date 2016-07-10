package numerals

case class NumeralsConverter() {

  val symbolsEquivalence: Map[Int, String] = Map(1 -> "I", 5 -> "V", 10 -> "X", 50 -> "L")
  val knownValues: List[Int] = symbolsEquivalence.keys.toList.reverse

  val fourDetector: FourDetector = new FourDetector
  val combinator: Combinator = new Combinator(knownValues)

  def apply(i: Int): String = {
    var simpleTranslation = getSymbolsTranslationOf(i)

    simpleTranslation = removeFourDuplicates(simpleTranslation)

    removedNonAllowedRepetitions(simpleTranslation, i)
  }

  private def getSymbolsTranslationOf(i: Int): String = {
    knownValues.find(i - _ >= 0) match {
      case Some(value) => symbolsEquivalence.get(value).get + getSymbolsTranslationOf(i - value)
      case None => ""
    }
  }

  private def remove4RepeatedCharsFrom(numeralsString: String): String = {
    fourDetector.get4Repeated(numeralsString) match {
      case Some(duplicate) => numeralsString.replace(duplicate, getSymbolReplacementFor(duplicate))
      case None => numeralsString
    }
  }

  private def getSymbolReplacementFor(repeatedChars: String): String = {
    getSymbolsCombinationFor(numeralToNumber(repeatedChars))
  }

  private def getSymbolsCombinationFor(value: Int): String = {
    combinator.findCombinationFor(value) match {
      case Some(combination) => getSymbolsTranslationOf(combination._1) + getSymbolsTranslationOf(combination._2)
      case None => ""
    }
  }

  private def numeralToNumber(numeral: String): Int = {
    val reverseMap: Map[String, Int] = for ((k, v) <- symbolsEquivalence) yield (v, k)
    charsListToStringList(numeral.toCharArray.reverse).flatMap(reverseMap.get).sum
  }

  private def charsListToStringList(chars: Array[Char]): List[String] = {
    chars.map(_.toString).toList
  }

  private def removeFourDuplicates(simpleTranslation: String): String = {
    var ret: String = simpleTranslation
    while (fourDetector.get4Repeated(ret).isDefined) {
      ret = remove4RepeatedCharsFrom(ret)
    }
    ret
  }

  private def removedNonAllowedRepetitions(numeralsString: String, value: Int): String = {
    findRepeated('V') in numeralsString match {
      case None => numeralsString
      case Some(repeatedSection) => numeralsString.replace(repeatedSection, getSymbolsCombinationFor(9))
    }
  }

}
