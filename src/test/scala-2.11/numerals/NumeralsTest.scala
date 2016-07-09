package numerals

import org.scalatest.{FlatSpec, Matchers}

class NumeralsTest extends FlatSpec with Matchers {

  val convertToNumeral: NumeralsConverter = NumeralsConverter()
  "The roman numeral converter" should "convert 1 to I" in {
    convertToNumeral(1) shouldBe "I"
  }

  it should "numbers smaller than 4 to I's" in {
    convertToNumeral(2) shouldBe "II"
    convertToNumeral(3) shouldBe "III"
  }

  it should "convert 4 to IV" in {
    convertToNumeral(4) shouldBe "IV"
  }

  it should "convert 5 to V" in {
    convertToNumeral(5) shouldBe "V"
  }

  it should "convert numbers larger than 5 by adding I's to V" in {
    convertToNumeral(6) shouldBe "VI"
    convertToNumeral(7) shouldBe "VII"
    convertToNumeral(8) shouldBe "VIII"
  }

  it should "convert 9 to IX" in {
    convertToNumeral(9) shouldBe "IX"
  }

  it should "convert 10 to X" in {
    convertToNumeral(10) shouldBe "X"
  }

  it should "convert numbers larger than 10 by adding I's to X" in {
    convertToNumeral(11) shouldBe "XI"
    convertToNumeral(12) shouldBe "XII"
    convertToNumeral(13) shouldBe "XIII"
  }

  it should "convert 14 and 19 using 5's and 10's rules" in {
    convertToNumeral(14) shouldBe "XIV"
    convertToNumeral(19) shouldBe "XIX"
  }

  it should "convert some numbers < 40" in {
    convertToNumeral(20) shouldBe "XX"
    convertToNumeral(24) shouldBe "XXIV"
    convertToNumeral(26) shouldBe "XXVI"
    convertToNumeral(39) shouldBe "XXXIX"
  }

  it should "convert 40 to XL" in {
    convertToNumeral(40) shouldBe "XL"
  }

  it should "convert some special 40 values" in {
    convertToNumeral(44) shouldBe "XLIV"
    convertToNumeral(45) shouldBe "XLV"
    convertToNumeral(46) shouldBe "XLVI"
    convertToNumeral(49) shouldBe "XLIX"
  }

  it should "convert 50 to L" in {
    convertToNumeral(50) shouldBe "L"
  }
}
