package numerals

import org.scalatest.{FlatSpec, Matchers}

class CombinatorTest extends FlatSpec with Matchers {

  var combinator: Combinator = new Combinator(List(1, 5, 10, 50, 100).reverse)

  "The combinator " should "be useful for roman numerals by finding combinations for special cases" in {
    combinator.findCombinationFor(4) shouldBe Some(1, 5)
    combinator.findCombinationFor(9) shouldBe Some(1, 10)
    combinator.findCombinationFor(40) shouldBe Some(10, 50)
    combinator.findCombinationFor(90) shouldBe Some(10, 100)
  }

  it should "return 0, 0 if no match is found" in {
    combinator.findCombinationFor(12) shouldBe None
  }
}
