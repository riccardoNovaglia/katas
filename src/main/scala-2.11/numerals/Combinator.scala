package numerals

class Combinator(someValues: List[Int]) {

  def findCombinationFor(i: Int): Option[(Int, Int)] = {
    findCombinationFor(i, someValues)
  }

  private def findCombinationFor(i: Int, values: List[Int]): Option[(Int, Int)] = {
    values match {
      case x :: xs => findEqualsIWhenXMinusXS(i, x, xs.reverse)
      case _ => None
    }
  }

  private def findEqualsIWhenXMinusXS(i: Int, x: Int, xs: List[Int]): Option[(Int, Int)] = {
    val decreased: List[Int] = xs.map(x - _)
    decreased.find(_ == i) match {
      case Some(sum) => Some(x - sum, x)
      case None => findCombinationFor(i, xs.reverse)
    }
  }
}
